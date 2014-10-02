package com.danaharrington.webcall

import play.api.libs.json._
import play.api.libs.ws._
import play.api.mvc.RequestHeader

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class CRequest[+T](cookies: CookieValues, body: T) {
  def map[U](f: T => U) = CRequest(cookies, f(body))
}

case class CResponse[+T](cookies: Seq[WSCookie], value: T) {
  def map[U](f: T => U) = CResponse(cookies, f(value))
}

case class CookieContext(ambient: CookieValues, local: Seq[WSCookie]) {
  def merge(newCookies: Seq[WSCookie]) = {
    def isShadowed(c: WSCookie): Boolean = c.name.exists(newCookies.flatMap(_.name).toSet contains _)
    val mergedLocalCookies = local.filterNot(isShadowed) ++ newCookies
    CookieContext(ambient, mergedLocalCookies)
  }

  def cookieValues: CookieValues = {
    ambient ++ local.toValues
  }

  def headers: Seq[(String, String)] = cookieValues.map{ case(key, value) => "Cookie" -> s"$key=$value"}.toSeq
}

object CookieContext {
  def apply(rh: RequestHeader): CookieContext = {
    CookieContext(rh.cookies.toSeq.map(c => c.name -> c.value).toMap, Seq.empty)
  }
}

/**
 * A web call which returns a value of type T
 * @tparam T
 */
class WebCall[T] private[webcall](val withCookies: CookieContext => WebResponse[T]) {

  /**
   * Manipulate the response value of a web call.
   * @param f A function to apply to the response value
   * @tparam U
   * @return A web call that applies `f` to its result.
   */
  def map[U](f: T => U): WebCall[U] = new WebCall[U]( cookieContext =>
    withCookies(cookieContext).map(_.map(f))
  )

  /**
   * Apply a web call to the result of the current web call
   * @param nextCall
   * @tparam U
   * @return A new web call that apply `this` and `nextCall` in sequence, threading the cookie values from `this` through
   *         `nextCall` and returning all new cookies in its result.
   */
  def flatMap[U](nextCall: T => WebCall[U]): WebCall[U] = new WebCall[U]( cookieContext =>
    for {
      CResponse(myCookies, myResponse) <- withCookies(cookieContext)
      newCookieContext = cookieContext.merge(myCookies)
      response <- nextCall(myResponse).withCookies(newCookieContext)
    } yield response
  )

  def execute()(implicit rh: RequestHeader): WebResponse[T] = {
    withCookies(CookieContext(rh))
  }

}

object WebCall {

  def unit[T](v: T) = new WebCall[T] ( (cc: CookieContext) =>
    Future.successful( CResponse(cc.local, v) )
  )

  /**
   * Create a cookie proxying web service call which takes an input
   * @param action The action to execute the call on an input
   * @param reqBody The input to the request
   * @tparam ReqT The type of the input
   * @tparam RespT The type of the response
   * @return The response from the web call
   */
  def webFn[ReqT, RespT](action: CRequest[ReqT] => WebResponse[RespT])(reqBody: ReqT): WebCall[RespT] =
    new WebCall[RespT]( cookieContext =>
      action(CRequest(cookieContext.cookieValues, reqBody)) map { case CResponse(newCookies, body) =>
        CResponse(cookieContext.merge(newCookies).local, body)
      }
    )

}

//object WsWebCall extends WsWebCall(WS.client)

class WsWebClient(ws: WSClient) {

  import WebCall.webFn

  private def wsrToJsonResponse(wsr: WSResponse) = CResponse(wsr.cookies, wsr.json)

  def get(url: String): WebCall[JsValue] = webFn[Unit, JsValue] { case CRequest(cv, _) =>
    ws.url(url).withHeaders(cv.toHeaders: _*).get().map(wsrToJsonResponse)
  }()

  def post[T](url: String)(body: T)(implicit wrt: play.api.http.Writeable[T], ct: play.api.http.ContentTypeOf[T]) =
    webFn[T, JsValue] { case CRequest(cv, reqBody) =>
      ws.url(url).withHeaders(cv.toHeaders: _*).post(reqBody).map(wsrToJsonResponse)
    }(body)
}

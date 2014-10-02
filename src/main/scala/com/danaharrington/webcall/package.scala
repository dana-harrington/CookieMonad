package com.danaharrington

import play.api.http.Writeable
import play.api.libs.json.{Json, Writes, JsValue}
import play.api.libs.ws.WSCookie
import play.api.mvc.{Cookie, Result}
import play.api.mvc.Results.Ok
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext


package object webcall {

  type CookieValues = Map[String, String]
  type WebResponse[T] = Future[CResponse[T]]

  implicit class toCookieValues(val cs: Seq[WSCookie]) extends AnyVal {
    // Oddly, a play.api.libs.ws.WSCookie can have either its name or value undefined.
    // We'll just drop any cookies that don't have both
    def toValues: CookieValues =
      cs.flatMap(c => for(name <- c.name; value <- c.value) yield name -> value).toMap
  }

  implicit class toHeaders(val cv: CookieValues) extends AnyVal {
    def toHeaders: Seq[(String, String)] = cv.map{ case (key, value) => "Cookie" -> s"$key=$value"}.toSeq
  }

  implicit class playResult[T](val wr: WebResponse[T]) extends AnyVal {
    private def cookieConvert(wsc: WSCookie): Option[Cookie] =
      for {
        name <- wsc.name
        value <- wsc.value
      } yield Cookie( name = name,
                      value = value,
                      path = wsc.path,
                      maxAge = wsc.maxAge,
                      secure = wsc.secure)


    def toResult(implicit wrt: Writeable[T]): Future[Result] = {
      wr.map { case CResponse(cookies, json) =>
        val playCookies = cookies.flatMap(cookieConvert)
        Ok(json).withCookies(playCookies: _*)
      }
    }

    def asJsonResult(implicit writes: Writes[T]): Future[Result] =
      wr.map(_.map(Json.toJson(_))).toResult
  }
}

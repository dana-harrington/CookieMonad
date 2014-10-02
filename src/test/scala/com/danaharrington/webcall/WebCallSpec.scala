package com.danaharrington.webcall

/**
 * Created by Dana on 2014-09-07.
 */

import org.specs2.mock._
import play.api.libs.json._
import play.api.libs.ws._
import play.api.test._
import play.api.http.{ContentTypeOf, Writeable}

import scala.concurrent.Future

class WebCallSpec extends PlaySpecification with Mockito {

  object TestHelpers {
    def createCookies( cs: (String, String)* ): Seq[WSCookie] = cs.map((createCookie _).tupled)

    def createCookie(cName: String, cValue: String): WSCookie = new WSCookie {
      def underlying[T]: T = ???
      def expires: Option[Long] = None
      def domain: String = ""
      def maxAge: Option[Int] = None
      def secure: Boolean = false
      def value: Option[String] = Some(cValue)
      def name: Option[String] = Some(cName)
      def path: String = ""
    }

    def mockResponse(cookies: Seq[WSCookie], json: JsValue): Future[WSResponse] = {
      val mockResponse = mock[WSResponse]
      mockResponse.cookies returns cookies
      mockResponse.json returns json
      Future.successful(mockResponse)
    }

    def setMockGetWsResponse(ws: WSClient, url: String, body: JsValue, resultCookies: Seq[WSCookie]): Unit = {
      val response = mockResponse(resultCookies, body)

      val endpoint = setMockEndpoint(ws, url)
      endpoint.get() returns response
    }

    def mockResponse(cookies: Seq[WSCookie], body: String): Future[WSResponse] = {
      val mockResponse = mock[WSResponse]
      mockResponse.cookies returns cookies
      mockResponse.body returns body
      Future.successful(mockResponse)
    }


    def setMockGetWsResponse(ws: WSClient, url: String, body: String, resultCookies: Seq[WSCookie]): Unit = {
      val response = mockResponse(resultCookies, body)

      val endpoint = setMockEndpoint(ws, url)
      endpoint.get() returns response
    }

    def setMockEndpoint(ws: WSClient, endpoint: String): WSRequestHolder = {
      val getRequestHolder = mock[WSRequestHolder]
      getRequestHolder.withHeaders(any) returns getRequestHolder
      ws.url(endpoint) returns getRequestHolder
      getRequestHolder
    }

    def setMockPostWsResponse(ws: WSClient, endpoint: String, body: String, resultCookies: Seq[WSCookie]): Unit = {
      val result = mockResponse(resultCookies, body)
      val getRequestHolder = mock[WSRequestHolder]
      getRequestHolder.withHeaders(any) returns getRequestHolder
      ws.url(endpoint) returns getRequestHolder
      getRequestHolder.get() returns result
    }
  }

  import TestHelpers._

  "A web call" should {
    "return the correct cookies and response body in a single call" in {
      running(FakeApplication()) {

        implicit val r = FakeRequest()
        val ws = mock[WSClient]
        val wsWebCall = new WsWebClient(ws)

        val getUrl = "someGetUrl"
        val postUrl = "somePostUrl"

        val getBody = Json.obj("id" -> 101)
        val serviceCookies = createCookies("c1" -> "v1")
        val resp = setMockGetWsResponse(ws, getUrl, getBody, serviceCookies)

        await(wsWebCall.get(getUrl).execute()) === CResponse(serviceCookies, getBody)
      }
    }

    "return the correct cookies and response in a sequence of calls" in {
      running(FakeApplication()) {

        implicit val r = FakeRequest()
        val ws = mock[WSClient]

        val getUrl = "someGetUrl"
        val postUrl1 = "somePostUrl"
        val postUrl2 = "anotherPostUrl"

        val getBody = Json.obj("id" -> 101)
        val getCookies = createCookies("c1" -> "v1", "c2" -> "v2")
        val gep = setMockEndpoint(ws, getUrl)
        gep.get() returns mockResponse(getCookies, getBody)

        val postCookies = createCookies("c2" -> "v22", "c3" -> "v3")
        val postBody = Json.obj("result" -> "good")
        setMockEndpoint(ws, postUrl1).post(any[JsValue])(any[Writeable[JsValue]], any[ContentTypeOf[JsValue]]) returns mockResponse(postCookies, postBody)

        val postCookies2 = createCookies("c2" -> "v23")
        val postBody2 = Json.obj("result" -> "fantastic")
        setMockEndpoint(ws, postUrl2).post(any[JsValue])(any[Writeable[JsValue]], any[ContentTypeOf[JsValue]]) returns mockResponse(postCookies2, postBody2)

        val wsWebCall = new WsWebClient(ws)

        def get1 = wsWebCall.get(getUrl)
        def post1(p: JsValue): WebCall[JsValue] = wsWebCall.post(postUrl1)(p)
        def post2(p: JsValue): WebCall[JsValue] = wsWebCall.post(postUrl2)(p)
        val webChain1 = for {
          resp1 <- get1
          resp2 <- post1(resp1)
        } yield {
          resp2
        }
        val CResponse(r1Cookies, r1Body) = await(webChain1.execute())
        r1Body === postBody
        r1Cookies.map(c => c.name.get -> c.value.get).toMap === Map("c1" -> "v1", "c2" -> "v22", "c3" -> "v3")

        val webChain2 = for {
          resp1 <- get1
          resp2 <- post1(resp1)
          post2Body = resp1.as[JsObject] ++ resp2.as[JsObject]
          resp3 <- post2(post2Body)
        } yield {
          resp3
        }
        val CResponse(r2Cookies, r2Body) = await(webChain2.execute())
        r2Body === postBody2
        r2Cookies.map(c => c.name.get -> c.value.get).toMap === Map("c1" -> "v1", "c2" -> "v23", "c3" -> "v3")

      }
    }
  }
}

object Examples {
  case class Sku(id: Long, productId: Long, name: String, price: BigDecimal)
  case class Product(id: Long, name: String, description: String, rating: BigDecimal)

  type SkuId = Long
  type Quantity = Int
  case class SkuQuantity(skuId: SkuId, quantity: Quantity)
  case class AddToCartRequest(items: Seq[SkuQuantity])
  case class AddToCartResponse(items: Seq[SkuQuantity])

  implicit val formatSku = Json.format[Sku]
  implicit val formatProduct = Json.format[Product]
  implicit val formatSkuQuantity = Json.format[SkuQuantity]
  implicit val formatAddToCartRequest = Json.format[AddToCartRequest]
  implicit val formatAddToCartResponse = Json.format[AddToCartResponse]

  import play.api.mvc.Action

  implicit val app = FakeApplication()

  val webClient = new WsWebClient(WS.client)

  def getSku(id: Long): WebCall[Sku] = webClient.get(s"/rest/sku/$id").map(_.as[Sku])
  def getProduct(id: Long): WebCall[Product] = webClient.get(s"/rest/product/$id").map(_.as[Product])
  def addToCart(items: (SkuId, Quantity)*): WebCall[AddToCartResponse] = {
    val request = AddToCartRequest(items.map((SkuQuantity.apply _).tupled))
    webClient.post("/rest/cart/add")(Json.toJson(request))
  }.map(_.as[AddToCartResponse])

  /**
   * Silly example: add an item to the cart only if its rating is above a threshold
   *   Chains together three calls:
   *     1. Get the SKU record (which contains the product id)
   *     2. Get the Product for the SKU (which contains the rating)
   *     3. Add the item to the cart (if conditions satisfied)
   * @param skuId SKU id
   * @param minAllowableRating Minimum value to permitting adding to cart
   * @param quantityToAdd Quantity to add if SKU's rating is >= minAllowableRating
   * @return An AddToCartResponse with the
   */
  def condAddSku(skuId: Long, minAllowableRating: Int, quantityToAdd: Int) = Action.async { implicit request =>

    def conditionalAddSku(skuId: SkuId): WebCall[AddToCartResponse] = for {
      sku <- getSku(skuId)
      product <- getProduct(sku.productId)
      cartAddResponse <-
        if (product.rating >= minAllowableRating)
          addToCart((skuId, quantityToAdd))
        else
          WebCall.unit(AddToCartResponse(Seq(SkuQuantity(skuId, 0))))
    } yield cartAddResponse

    conditionalAddSku(skuId).execute().asJsonResult
  }

}
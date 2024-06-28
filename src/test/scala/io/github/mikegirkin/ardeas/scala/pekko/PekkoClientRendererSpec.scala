package io.github.mikegirkin.ardeas.scala.pekko

import io.github.mikegirkin.ardeas.ParsedSimplePetStore
import io.github.mikegirkin.ardeas.scala.pekko.PekkoClientRenderer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PekkoClientRendererSpec extends AnyWordSpec with Matchers {
  "PekkoClientRenderer" should {
    "be able to render simple PetStore" in {

      val renderResult = PekkoClientRenderer.renderClient(ParsedSimplePetStore.api, Some("test"), List("org.http4s.circe._"))
      val expected =
        """package test
          |
          |import org.apache.pekko.actor.ActorSystem
          |import org.apache.pekko.http.scaladsl.Http
          |import org.apache.pekko.http.scaladsl.marshalling.Marshal
          |import org.apache.pekko.http.scaladsl.model.{HttpEntity, RequestEntity, HttpHeader, HttpMethods, HttpRequest, HttpResponse, ResponseEntity, StatusCodes, Uri}
          |import org.apache.pekko.http.scaladsl.model.Uri.Query
          |import org.apache.pekko.http.scaladsl.unmarshalling.Unmarshal
          |import Components.Schemas._
          |import scala.concurrent.ExecutionContext
          |import scala.concurrent.Future
          |import org.http4s.circe._
          |
          |object Responses {
          |  case class ResponseData(
          |    raw: HttpResponse
          |  ) {
          |    def headers: Seq[HttpHeader] = raw.headers
          |  }
          |
          |  sealed trait ListPetsResponse extends Product with Serializable
          |  final case class ListPetsResponse200(
          |    content: Pets,
          |    responseData: ResponseData
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData
          |  ) extends ListPetsResponse
          |
          |  sealed trait CreatePetsResponse extends Product with Serializable
          |  final case class CreatePetsResponse201(
          |    responseData: ResponseData
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData
          |  ) extends CreatePetsResponse
          |
          |  sealed trait UpdatePetResponse extends Product with Serializable
          |  final case class UpdatePetResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    responseData: ResponseData
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData
          |  ) extends UpdatePetResponse
          |
          |  sealed trait ShowPetByIdResponse extends Product with Serializable
          |  final case class ShowPetByIdResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    responseData: ResponseData
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData
          |  ) extends ShowPetByIdResponse
          |
          |  sealed trait LogoutResponse extends Product with Serializable
          |  final case class LogoutResponse405(
          |    allowMethods: Set[org.http4s.Method],
          |    responseData: ResponseData
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDefault(
          |    httpCode: Int,
          |    responseData: ResponseData
          |  ) extends LogoutResponse
          |}
          |
          |trait Client {
          |  def listPets(limit: Option[Int], headers: Seq[HttpHeader] = Seq.empty): Future[ListPetsResponse]
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[HttpHeader] = Seq.empty): Future[CreatePetsResponse]
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[HttpHeader] = Seq.empty): Future[UpdatePetResponse]
          |  def showPetById(petId: Int, headers: Seq[HttpHeader] = Seq.empty): Future[ShowPetByIdResponse]
          |  def logout(headers: Seq[HttpHeader] = Seq.empty): Future[LogoutResponse]
          |}
          |
          |class ClientImpl(
          |  baseUri: Uri,
          |  defaultHeaders: Seq[HttpHeader] = Seq.empty
          |) ( implicit
          |  ec: ExecutionContext,
          |  as: ActorSystem
          |) extends Client {
          |
          |import Responses._
          |
          |  def listPets(limit: Option[Int], headers: Seq[HttpHeader] = Seq.empty): Future[ListPetsResponse] = {
          |    val queryParameters = Map.empty[String, String]
          |      .concat(limit.map(x => "limit" -> x.toString))
          |    val requestUrl = baseUri.withPath(baseUri.path / "pets")
          |      .withQuery(Query(queryParameters))
          |    for {
          |      request <- Future.successful(
          |        HttpRequest(
          |          HttpMethods.GET,
          |          requestUrl,
          |          headers = defaultHeaders.appended(headers),
          |          HttpEntity.Empty
          |        )
          |      )
          |      rawResponse <- Http().singleRequest(request)
          |      result <- rawResponse.status match {
          |        case StatusCodes.OK =>
          |          Unmarshal(rawResponse.entity).to[Pets]
          |            .map { entity =>
          |              Responses.ListPetsResponse200(entity, ResponseData(rawResponse))
          |            }
          |        case _ =>
          |          Unmarshal(rawResponse.entity).to[Error]
          |            .map { entity =>
          |              Responses.ListPetsResponseDefault(entity, ResponseData(rawResponse))
          |            }
          |      }
          |    } yield {
          |      result
          |    }
          |  }
          |
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[HttpHeader] = Seq.empty): Future[CreatePetsResponse] = {
          |    val requestUrl = baseUri.withPath(baseUri.path / "pets")
          |    for {
          |      entity <- Marshal(body).to[RequestEntity]
          |      request = HttpRequest(
          |        HttpMethods.POST,
          |        requestUrl,
          |        headers = defaultHeaders.appended(headers),
          |        entity
          |      )
          |      rawResponse <- Http().singleRequest(request)
          |      result <- rawResponse.status match {
          |        case StatusCodes.Created =>
          |          Future.successful(
          |            Responses.CreatePetsResponse201(ResponseData(rawResponse))
          |          )
          |        case _ =>
          |          Unmarshal(rawResponse.entity).to[Error]
          |            .map { entity =>
          |              Responses.CreatePetsResponseDefault(entity, ResponseData(rawResponse))
          |            }
          |      }
          |    } yield {
          |      result
          |    }
          |  }
          |
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[HttpHeader] = Seq.empty): Future[UpdatePetResponse] = {
          |    val requestUrl = baseUri.withPath(baseUri.path / "pets" / petId.toString)
          |    for {
          |      entity <- Marshal(body).to[RequestEntity]
          |      request = HttpRequest(
          |        HttpMethods.PUT,
          |        requestUrl,
          |        headers = defaultHeaders.appended(headers),
          |        entity
          |      )
          |      rawResponse <- Http().singleRequest(request)
          |      result <- rawResponse.status match {
          |        case StatusCodes.OK =>
          |          Unmarshal(rawResponse.entity).to[Components.Responses.SinglePetResponse]
          |            .map { entity =>
          |              Responses.UpdatePetResponse200(entity, ResponseData(rawResponse))
          |            }
          |        case _ =>
          |          Unmarshal(rawResponse.entity).to[Error]
          |            .map { entity =>
          |              Responses.UpdatePetResponseDefault(entity, ResponseData(rawResponse))
          |            }
          |      }
          |    } yield {
          |      result
          |    }
          |  }
          |
          |  def showPetById(petId: Int, headers: Seq[HttpHeader] = Seq.empty): Future[ShowPetByIdResponse] = {
          |    val requestUrl = baseUri.withPath(baseUri.path / "pets" / petId.toString)
          |    for {
          |      request <- Future.successful(
          |        HttpRequest(
          |          HttpMethods.GET,
          |          requestUrl,
          |          headers = defaultHeaders.appended(headers),
          |          HttpEntity.Empty
          |        )
          |      )
          |      rawResponse <- Http().singleRequest(request)
          |      result <- rawResponse.status match {
          |        case StatusCodes.OK =>
          |          Unmarshal(rawResponse.entity).to[Components.Responses.SinglePetResponse]
          |            .map { entity =>
          |              Responses.ShowPetByIdResponse200(entity, ResponseData(rawResponse))
          |            }
          |        case _ =>
          |          Unmarshal(rawResponse.entity).to[Error]
          |            .map { entity =>
          |              Responses.ShowPetByIdResponseDefault(entity, ResponseData(rawResponse))
          |            }
          |      }
          |    } yield {
          |      result
          |    }
          |  }
          |
          |  def logout(headers: Seq[HttpHeader] = Seq.empty): Future[LogoutResponse] = {
          |    val requestUrl = baseUri.withPath(baseUri.path / "logout")
          |    for {
          |      request <- Future.successful(
          |        HttpRequest(
          |          HttpMethods.POST,
          |          requestUrl,
          |          headers = defaultHeaders.appended(headers),
          |          HttpEntity.Empty
          |        )
          |      )
          |      rawResponse <- Http().singleRequest(request)
          |      result <- rawResponse.status match {
          |        case StatusCodes.MethodNotAllowed =>
          |          Future.successful(
          |            Responses.LogoutResponse405(ResponseData(rawResponse))
          |          )
          |        case _ =>
          |          Future.successful(
          |            Responses.LogoutResponseDefault(ResponseData(rawResponse))
          |          )
          |      }
          |    } yield {
          |      result
          |    }
          |  }
          |}
          |""".stripMargin

      renderResult shouldBe expected


    }
  }
}

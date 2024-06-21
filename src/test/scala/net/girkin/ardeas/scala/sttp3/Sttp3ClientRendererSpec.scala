package net.girkin.ardeas.scala.sttp3

import net.girkin.ardeas.ParsedSimplePetStore
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Sttp3ClientRendererSpec extends AnyWordSpec with Matchers {
  "Sttp3ClientRenderer" should {
    "be able to render simple PetStore" in {

      val renderResult = Sttp3ClientRenderer.renderClient(ParsedSimplePetStore.api, Some("test"), List("org.http4s.circe._"))
      val expected =
        """package test
          |
          |import sttp.client3._
          |import sttp.model._
          |import Components.Schemas._
          |import org.http4s.circe._
          |
          |object Responses {
          |  sealed trait ListPetsResponse extends Product with Serializable {
          |    def headers: Seq[Header]
          |    def statusCode: Int
          |  }
          |  final case class ListPetsResponse200(
          |    content: Pets,
          |    headers: Seq[Header]
          |  ) extends ListPetsResponse {
          |    val statusCode = 200
          |  }
          |  final case class ListPetsResponseDefault(
          |    content: Error,
          |    headers: Seq[Header]
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDeserializationError[Err](
          |    content: String,
          |    statusCode: Int,
          |    headers: Seq[Header],
          |    error: Err
          |  ) extends ListPetsResponse
          |
          |  sealed trait CreatePetsResponse extends Product with Serializable {
          |    def headers: Seq[Header]
          |    def statusCode: Int
          |  }
          |  final case class CreatePetsResponse201(
          |    headers: Seq[Header]
          |  ) extends CreatePetsResponse {
          |    val statusCode = 201
          |  }
          |  final case class CreatePetsResponseDefault(
          |    content: Error,
          |    headers: Seq[Header]
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDeserializationError[Err](
          |    content: String,
          |    statusCode: Int,
          |    headers: Seq[Header],
          |    error: Err
          |  ) extends CreatePetsResponse
          |
          |  sealed trait UpdatePetResponse extends Product with Serializable {
          |    def headers: Seq[Header]
          |    def statusCode: Int
          |  }
          |  final case class UpdatePetResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    headers: Seq[Header]
          |  ) extends UpdatePetResponse {
          |    val statusCode = 200
          |  }
          |  final case class UpdatePetResponseDefault(
          |    content: Error,
          |    headers: Seq[Header]
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDeserializationError[Err](
          |    content: String,
          |    statusCode: Int,
          |    headers: Seq[Header],
          |    error: Err
          |  ) extends UpdatePetResponse
          |
          |  sealed trait ShowPetByIdResponse extends Product with Serializable {
          |    def headers: Seq[Header]
          |    def statusCode: Int
          |  }
          |  final case class ShowPetByIdResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    headers: Seq[Header]
          |  ) extends ShowPetByIdResponse {
          |    val statusCode = 200
          |  }
          |  final case class ShowPetByIdResponseDefault(
          |    content: Error,
          |    headers: Seq[Header]
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDeserializationError[Err](
          |    content: String,
          |    statusCode: Int,
          |    headers: Seq[Header],
          |    error: Err
          |  ) extends ShowPetByIdResponse
          |
          |  sealed trait LogoutResponse extends Product with Serializable {
          |    def headers: Seq[Header]
          |    def statusCode: Int
          |  }
          |  final case class LogoutResponse405(
          |    headers: Seq[Header]
          |  ) extends LogoutResponse {
          |    val statusCode = 405
          |  }
          |  final case class LogoutResponseDefault(
          |    headers: Seq[Header]
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDeserializationError[Err](
          |    content: String,
          |    statusCode: Int,
          |    headers: Seq[Header],
          |    error: Err
          |  ) extends LogoutResponse
          |}
          |
          |trait Client[F[_]] {
          | import Responses._
          |
          |  def listPets(limit: Option[Int], headers: Seq[Header] = Seq.empty): F[ListPetsResponse]
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[Header] = Seq.empty): F[CreatePetsResponse]
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[Header] = Seq.empty): F[UpdatePetResponse]
          |  def showPetById(petId: Int, headers: Seq[Header] = Seq.empty): F[ShowPetByIdResponse]
          |  def logout(headers: Seq[Header] = Seq.empty): F[LogoutResponse]
          |}
          |
          |class ClientImpl[F[_], P](
          |  baseUri: Uri,
          |  backend: SttpBackend[F, P],
          |  defaultHeaders: Seq[Header] = Seq.empty
          |) extends Client[F] {
          |  import Responses._
          |
          |  def listPets(limit: Option[Int], headers: Seq[Header] = Seq.empty): F[ListPetsResponse] = {
          |    val queryParameters = List.empty[(String, String)]
          |      .appendedAll(limit.map(item => "limit" -> item.toString))
          |    val url = uri"$baseUri/pets"
          |      .withParams(QueryParams.fromSeq(queryParameters))
          |    val responseF = basicRequest
          |      .get(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(asStringAlways)
          |      .send(backend)
          |
          |    backend.responseMonad.map(responseF) { response =>
          |      response.code.code match {
          |        case 200 =>
          |          deserialize[Pets](response.body).fold(
          |            error => Responses.ListPetsResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.ListPetsResponse200(entity, response.headers)
          |          )
          |        case _ =>
          |          deserialize[Error](response.body).fold(
          |            error => Responses.ListPetsResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.ListPetsResponseDefault(entity, response.headers)
          |          )
          |      }
          |    }
          |  }
          |
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[Header] = Seq.empty): F[CreatePetsResponse] = {
          |    val url = uri"$baseUri/pets"
          |    val responseF = basicRequest
          |      .post(url)
          |      .body(serialize(body))
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(asStringAlways)
          |      .send(backend)
          |
          |    backend.responseMonad.map(responseF) { response =>
          |      response.code.code match {
          |        case 201 =>
          |          Responses.CreatePetsResponse201(response.headers)
          |        case _ =>
          |          deserialize[Error](response.body).fold(
          |            error => Responses.CreatePetsResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.CreatePetsResponseDefault(entity, response.headers)
          |          )
          |      }
          |    }
          |  }
          |
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[Header] = Seq.empty): F[UpdatePetResponse] = {
          |    val url = uri"$baseUri/pets/$petId"
          |    val responseF = basicRequest
          |      .put(url)
          |      .body(serialize(body))
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(asStringAlways)
          |      .send(backend)
          |
          |    backend.responseMonad.map(responseF) { response =>
          |      response.code.code match {
          |        case 200 =>
          |          deserialize[Components.Responses.SinglePetResponse](response.body).fold(
          |            error => Responses.UpdatePetResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.UpdatePetResponse200(entity, response.headers)
          |          )
          |        case _ =>
          |          deserialize[Error](response.body).fold(
          |            error => Responses.UpdatePetResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.UpdatePetResponseDefault(entity, response.headers)
          |          )
          |      }
          |    }
          |  }
          |
          |  def showPetById(petId: Int, headers: Seq[Header] = Seq.empty): F[ShowPetByIdResponse] = {
          |    val url = uri"$baseUri/pets/$petId"
          |    val responseF = basicRequest
          |      .get(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(asStringAlways)
          |      .send(backend)
          |
          |    backend.responseMonad.map(responseF) { response =>
          |      response.code.code match {
          |        case 200 =>
          |          deserialize[Components.Responses.SinglePetResponse](response.body).fold(
          |            error => Responses.ShowPetByIdResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.ShowPetByIdResponse200(entity, response.headers)
          |          )
          |        case _ =>
          |          deserialize[Error](response.body).fold(
          |            error => Responses.ShowPetByIdResponseDeserializationError(response.body, response.code.code, response.headers, error),
          |            entity => Responses.ShowPetByIdResponseDefault(entity, response.headers)
          |          )
          |      }
          |    }
          |  }
          |
          |  def logout(headers: Seq[Header] = Seq.empty): F[LogoutResponse] = {
          |    val url = uri"$baseUri/logout"
          |    val responseF = basicRequest
          |      .post(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(asStringAlways)
          |      .send(backend)
          |
          |    backend.responseMonad.map(responseF) { response =>
          |      response.code.code match {
          |        case 405 =>
          |          Responses.LogoutResponse405(response.headers)
          |        case _ =>
          |          Responses.LogoutResponseDefault(response.headers)
          |      }
          |    }
          |  }
          |}
          |""".stripMargin

      renderResult shouldBe expected
    }
  }
}

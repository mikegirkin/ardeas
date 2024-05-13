package net.girkin.ardeas.scala.sttp3

import net.girkin.ardeas.ParsedSimplePetStore
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Sttp3ClientRendererSpec extends AnyWordSpec with Matchers {
  "PekkoClientRenderer" should {
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
          |  sealed trait ListPetsResponse extends Product with Serializable
          |  final case class ListPetsResponse200(
          |    content: Pets
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDefault(
          |    content: Error
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDeserializationError[Err](
          |    content: String,
          |    error: Err
          |  ) extends ListPetsResponse
          |
          |  sealed trait CreatePetsResponse extends Product with Serializable
          |  final case class CreatePetsResponse201(
          |
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDefault(
          |    content: Error
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDeserializationError[Err](
          |    content: String,
          |    error: Err
          |  ) extends CreatePetsResponse
          |
          |  sealed trait UpdatePetResponse extends Product with Serializable
          |  final case class UpdatePetResponse200(
          |    content: Components.Responses.SinglePetResponse
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDefault(
          |    content: Error
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDeserializationError[Err](
          |    content: String,
          |    error: Err
          |  ) extends UpdatePetResponse
          |
          |  sealed trait ShowPetByIdResponse extends Product with Serializable
          |  final case class ShowPetByIdResponse200(
          |    content: Components.Responses.SinglePetResponse
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDefault(
          |    content: Error
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDeserializationError[Err](
          |    content: String,
          |    error: Err
          |  ) extends ShowPetByIdResponse
          |
          |  sealed trait LogoutResponse extends Product with Serializable
          |  final case class LogoutResponse405(
          |
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDefault(
          |
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDeserializationError[Err](
          |    content: String,
          |    error: Err
          |  ) extends LogoutResponse
          |}
          |
          |trait Client[F[_]] {
          | import Responses._
          |
          |  def listPets(limit: Option[Int], headers: Seq[Header] = Seq.empty): F[Response[ListPetsResponse]]
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[Header] = Seq.empty): F[Response[CreatePetsResponse]]
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[Header] = Seq.empty): F[Response[UpdatePetResponse]]
          |  def showPetById(petId: Int, headers: Seq[Header] = Seq.empty): F[Response[ShowPetByIdResponse]]
          |  def logout(headers: Seq[Header] = Seq.empty): F[Response[LogoutResponse]]
          |}
          |
          |class ClientImpl[F[_], P](
          |  baseUri: Uri,
          |  backend: SttpBackend[F, P],
          |  defaultHeaders: Seq[Header] = Seq.empty
          |) extends Client[F] {
          |  import Responses._
          |
          |  def listPets(limit: Option[Int], headers: Seq[Header] = Seq.empty): F[Response[ListPetsResponse]] = {
          |    val queryParameters = List.empty[(String, String)]
          |      .appendedAll(limit.map(item => "limit" -> item.toString))
          |    val url = uri"$baseUri/pets"
          |      .withParams(QueryParams.fromSeq(queryParameters))
          |    basicRequest
          |      .get(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(fromMetadata(
          |        asStringAlways.map[Responses.ListPetsResponse] { content =>
          |          deserialize[Error](content).fold(
          |            error => Responses.ListPetsResponseDeserializationError(content, error),
          |            entity => Responses.ListPetsResponseDefault(entity)
          |          )
          |        },
          |        ConditionalResponseAs(_.code.code == 200, asStringAlways.map[Responses.ListPetsResponse] { content =>
          |          deserialize[Pets](content).fold(
          |            error => Responses.ListPetsResponseDeserializationError(content, error),
          |            entity => Responses.ListPetsResponse200(entity)
          |          )
          |        })
          |      ))
          |      .send(backend)
          |  }
          |
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Seq[Header] = Seq.empty): F[Response[CreatePetsResponse]] = {
          |    val url = uri"$baseUri/pets"
          |    basicRequest
          |      .post(url)
          |      .body(serialize(body))
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(fromMetadata(
          |        asStringAlways.map[Responses.CreatePetsResponse] { content =>
          |          deserialize[Error](content).fold(
          |            error => Responses.CreatePetsResponseDeserializationError(content, error),
          |            entity => Responses.CreatePetsResponseDefault(entity)
          |          )
          |        },
          |        ConditionalResponseAs(_.code.code == 201, asStringAlways.map[Responses.CreatePetsResponse] { content =>
          |          Responses.CreatePetsResponse201()
          |        })
          |      ))
          |      .send(backend)
          |  }
          |
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Seq[Header] = Seq.empty): F[Response[UpdatePetResponse]] = {
          |    val url = uri"$baseUri/pets/$petId"
          |    basicRequest
          |      .put(url)
          |      .body(serialize(body))
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(fromMetadata(
          |        asStringAlways.map[Responses.UpdatePetResponse] { content =>
          |          deserialize[Error](content).fold(
          |            error => Responses.UpdatePetResponseDeserializationError(content, error),
          |            entity => Responses.UpdatePetResponseDefault(entity)
          |          )
          |        },
          |        ConditionalResponseAs(_.code.code == 200, asStringAlways.map[Responses.UpdatePetResponse] { content =>
          |          deserialize[Components.Responses.SinglePetResponse](content).fold(
          |            error => Responses.UpdatePetResponseDeserializationError(content, error),
          |            entity => Responses.UpdatePetResponse200(entity)
          |          )
          |        })
          |      ))
          |      .send(backend)
          |  }
          |
          |  def showPetById(petId: Int, headers: Seq[Header] = Seq.empty): F[Response[ShowPetByIdResponse]] = {
          |    val url = uri"$baseUri/pets/$petId"
          |    basicRequest
          |      .get(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(fromMetadata(
          |        asStringAlways.map[Responses.ShowPetByIdResponse] { content =>
          |          deserialize[Error](content).fold(
          |            error => Responses.ShowPetByIdResponseDeserializationError(content, error),
          |            entity => Responses.ShowPetByIdResponseDefault(entity)
          |          )
          |        },
          |        ConditionalResponseAs(_.code.code == 200, asStringAlways.map[Responses.ShowPetByIdResponse] { content =>
          |          deserialize[Components.Responses.SinglePetResponse](content).fold(
          |            error => Responses.ShowPetByIdResponseDeserializationError(content, error),
          |            entity => Responses.ShowPetByIdResponse200(entity)
          |          )
          |        })
          |      ))
          |      .send(backend)
          |  }
          |
          |  def logout(headers: Seq[Header] = Seq.empty): F[Response[LogoutResponse]] = {
          |    val url = uri"$baseUri/logout"
          |    basicRequest
          |      .post(url)
          |      .headers(defaultHeaders.concat(headers):_*)
          |      .response(fromMetadata(
          |        asStringAlways.map[Responses.LogoutResponse] { content =>
          |          Responses.LogoutResponseDefault()
          |        },
          |        ConditionalResponseAs(_.code.code == 405, asStringAlways.map[Responses.LogoutResponse] { content =>
          |          Responses.LogoutResponse405()
          |        })
          |      ))
          |      .send(backend)
          |  }
          |}
          |""".stripMargin

      renderResult shouldBe expected
    }
  }
}

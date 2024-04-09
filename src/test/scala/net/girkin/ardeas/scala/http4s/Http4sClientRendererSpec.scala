package net.girkin.ardeas.scala.http4s

import net.girkin.ardeas.ParsedSimplePetStore
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Http4sClientRendererSpec extends AnyWordSpec with Matchers {
  "Http4s client renderer" should {
    "be able to render simple petstore" in {

      val renderResult = Http4sClientRenderer.renderClient(ParsedSimplePetStore.api, Some("test"), List("org.http4s.circe._"))
      val expected =
        """package test
          |
          |import cats.effect.Concurrent
          |import cats.Applicative
          |import org.http4s._
          |import org.http4s.Method._
          |import org.http4s.client.dsl.Http4sClientDsl
          |import Components.Schemas._
          |import org.http4s.circe._
          |
          |object Responses {
          |  case class ResponseData[F[_]](
          |    raw: Response[F]
          |  ) {
          |    def headers: Headers = raw.headers
          |  }
          |
          |  sealed trait ListPetsResponse extends Product with Serializable
          |  final case class ListPetsResponse200[F[_]](
          |    content: Pets,
          |    responseData: ResponseData[F]
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDefault[F[_]](
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData[F]
          |  ) extends ListPetsResponse
          |
          |  sealed trait CreatePetsResponse extends Product with Serializable
          |  final case class CreatePetsResponse201[F[_]](
          |    responseData: ResponseData[F]
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDefault[F[_]](
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData[F]
          |  ) extends CreatePetsResponse
          |
          |  sealed trait UpdatePetResponse extends Product with Serializable
          |  final case class UpdatePetResponse200[F[_]](
          |    content: Components.Responses.SinglePetResponse,
          |    responseData: ResponseData[F]
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDefault[F[_]](
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData[F]
          |  ) extends UpdatePetResponse
          |
          |  sealed trait ShowPetByIdResponse extends Product with Serializable
          |  final case class ShowPetByIdResponse200[F[_]](
          |    content: Components.Responses.SinglePetResponse,
          |    responseData: ResponseData[F]
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDefault[F[_]](
          |    httpCode: Int,
          |    content: Error,
          |    responseData: ResponseData[F]
          |  ) extends ShowPetByIdResponse
          |
          |  sealed trait LogoutResponse extends Product with Serializable
          |  final case class LogoutResponse405[F[_]](
          |    allowMethods: Set[org.http4s.Method],
          |    responseData: ResponseData[F]
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDefault[F[_]](
          |    httpCode: Int,
          |    responseData: ResponseData[F]
          |  ) extends LogoutResponse
          |}
          |
          |trait Client[F[_]] {
          |  def listPets(limit: Option[Int], headers: Headers = Headers.empty): F[ListPetsResponse]
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Headers = Headers.empty): F[CreatePetsResponse]
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Headers = Headers.empty): F[UpdatePetResponse]
          |  def showPetById(petId: Int, headers: Headers = Headers.empty): F[ShowPetByIdResponse]
          |  def logout(headers: Headers = Headers.empty): F[LogoutResponse]
          |}
          |
          |class ClientImpl[F[_]: Concurrent](
          |  client: org.http4s.client.Client[F],
          |  baseUri: Uri,
          |  defaultHeaders: Headers = Headers.empty
          |) extends Client[F] {
          |  import cats.syntax.functor._
          |  import org.http4s.Status._
          |  import Responses._
          |
          |  private val clientDsl = new Http4sClientDsl[F] {}
          |  import clientDsl._
          |
          |  def listPets(limit: Option[Int], headers: Headers = Headers.empty): F[ListPetsResponse] = {
          |    val url = (baseUri / "pets")
          |      .withOptionQueryParam("limit", limit)
          |
          |    val request = GET(url)
          |      .withHeaders(defaultHeaders ++ headers)
          |
          |    client.run(request).use { response =>
          |      response.status match {
          |        case Ok => for {
          |          content <- response.as[Pets]
          |        } yield {
          |          ListPetsResponse200(content, ResponseData(response))
          |        }
          |        case _ => for {
          |          content <- response.as[Error]
          |        } yield {
          |          ListPetsResponseDefault(response.status.code, content, ResponseData(response))
          |        }
          |      }
          |    }
          |  }
          |
          |  def createPets(body: Components.RequestBodies.CreatePetRequest, headers: Headers = Headers.empty): F[CreatePetsResponse] = {
          |    val url = (baseUri / "pets")
          |
          |    val request = POST(url)
          |      .withEntity(body)
          |      .withHeaders(defaultHeaders ++ headers)
          |
          |    client.run(request).use { response =>
          |      response.status match {
          |        case Created => Applicative[F].pure(
          |          CreatePetsResponse201(ResponseData(response))
          |        )
          |        case _ => for {
          |          content <- response.as[Error]
          |        } yield {
          |          CreatePetsResponseDefault(response.status.code, content, ResponseData(response))
          |        }
          |      }
          |    }
          |  }
          |
          |  def updatePet(petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Headers = Headers.empty): F[UpdatePetResponse] = {
          |    val url = (baseUri / "pets" / petId.toString)
          |
          |    val request = PUT(url)
          |      .withEntity(body)
          |      .withHeaders(defaultHeaders ++ headers)
          |
          |    client.run(request).use { response =>
          |      response.status match {
          |        case Ok => for {
          |          content <- response.as[Components.Responses.SinglePetResponse]
          |        } yield {
          |          UpdatePetResponse200(content, ResponseData(response))
          |        }
          |        case _ => for {
          |          content <- response.as[Error]
          |        } yield {
          |          UpdatePetResponseDefault(response.status.code, content, ResponseData(response))
          |        }
          |      }
          |    }
          |  }
          |
          |  def showPetById(petId: Int, headers: Headers = Headers.empty): F[ShowPetByIdResponse] = {
          |    val url = (baseUri / "pets" / petId.toString)
          |
          |    val request = GET(url)
          |      .withHeaders(defaultHeaders ++ headers)
          |
          |    client.run(request).use { response =>
          |      response.status match {
          |        case Ok => for {
          |          content <- response.as[Components.Responses.SinglePetResponse]
          |        } yield {
          |          ShowPetByIdResponse200(content, ResponseData(response))
          |        }
          |        case _ => for {
          |          content <- response.as[Error]
          |        } yield {
          |          ShowPetByIdResponseDefault(response.status.code, content, ResponseData(response))
          |        }
          |      }
          |    }
          |  }
          |
          |  def logout(headers: Headers = Headers.empty): F[LogoutResponse] = {
          |    val url = (baseUri / "logout")
          |
          |    val request = POST(url)
          |      .withHeaders(defaultHeaders ++ headers)
          |
          |    client.run(request).use { response =>
          |      response.status match {
          |        case MethodNotAllowed => Applicative[F].pure(
          |          LogoutResponse405(response.headers.get[org.http4s.headers.Allow].map(_.methods).getOrElse(Set.empty), ResponseData(response))
          |        )
          |        case _ => Applicative[F].pure(
          |          LogoutResponseDefault(response.status.code, ResponseData(response))
          |        )
          |      }
          |    }
          |  }
          |}
          |""".stripMargin

      renderResult shouldBe expected
    }
  }
}

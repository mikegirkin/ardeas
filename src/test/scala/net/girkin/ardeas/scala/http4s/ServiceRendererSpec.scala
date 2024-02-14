package net.girkin.ardeas.scala.http4s

import net.girkin.ardeas.ParsedSimplePetStore
import net.girkin.ardeas.scala.http4s.ServiceRenderer
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class ServiceRendererSpec extends AnyWordSpec with should.Matchers {
  "Http4s service renderer" should {
    "be able to render simple petstore" in {
      val renderResult = ServiceRenderer.renderService(ParsedSimplePetStore.api, Some("test"), List("org.http4s.circe._"))
      val expected =
        """package test
          |
          |import cats.effect.Concurrent
          |import cats.Applicative
          |import cats.implicits._
          |import org.http4s.{HttpRoutes, Headers, Request, Response}
          |import org.http4s.dsl.Http4sDsl
          |import Components.Schemas._
          |import org.http4s.circe._
          |
          |import scala.annotation.nowarn
          |
          |object Responses {
          |  sealed trait ListPetsResponse extends Product with Serializable
          |  final case class ListPetsResponse200(
          |    content: Pets,
          |    headers: Headers
          |  ) extends ListPetsResponse
          |  final case class ListPetsResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    headers: Headers
          |  ) extends ListPetsResponse
          |
          |  sealed trait CreatePetsResponse extends Product with Serializable
          |  final case class CreatePetsResponse201(
          |    headers: Headers
          |  ) extends CreatePetsResponse
          |  final case class CreatePetsResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    headers: Headers
          |  ) extends CreatePetsResponse
          |
          |  sealed trait UpdatePetResponse extends Product with Serializable
          |  final case class UpdatePetResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    headers: Headers
          |  ) extends UpdatePetResponse
          |  final case class UpdatePetResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    headers: Headers
          |  ) extends UpdatePetResponse
          |
          |  sealed trait ShowPetByIdResponse extends Product with Serializable
          |  final case class ShowPetByIdResponse200(
          |    content: Components.Responses.SinglePetResponse,
          |    headers: Headers
          |  ) extends ShowPetByIdResponse
          |  final case class ShowPetByIdResponseDefault(
          |    httpCode: Int,
          |    content: Error,
          |    headers: Headers
          |  ) extends ShowPetByIdResponse
          |
          |  sealed trait LogoutResponse extends Product with Serializable
          |  final case class LogoutResponse405(
          |    allowMethods: Set[org.http4s.Method],
          |    headers: Headers
          |  ) extends LogoutResponse
          |  final case class LogoutResponseDefault(
          |    httpCode: Int,
          |    headers: Headers
          |  ) extends LogoutResponse
          |}
          |
          |@nowarn class ResponseConverters[F[_]: Concurrent](dsl: Http4sDsl[F]) {
          |  import Responses._
          |  import dsl._
          |  import org.http4s.Response
          |
          |  def asHttp4sResponse(response: ListPetsResponse): F[Response[F]] = {
          |    response match {
          |      case r: ListPetsResponse200 => Ok(r.content)
          |      case r: ListPetsResponseDefault => implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode)).withEntity(r.content))
          |    }
          |  }
          |  def asHttp4sResponse(response: CreatePetsResponse): F[Response[F]] = {
          |    response match {
          |      case r: CreatePetsResponse201 => Created()
          |      case r: CreatePetsResponseDefault => implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode)).withEntity(r.content))
          |    }
          |  }
          |  def asHttp4sResponse(response: UpdatePetResponse): F[Response[F]] = {
          |    response match {
          |      case r: UpdatePetResponse200 => Ok(r.content)
          |      case r: UpdatePetResponseDefault => implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode)).withEntity(r.content))
          |    }
          |  }
          |  def asHttp4sResponse(response: ShowPetByIdResponse): F[Response[F]] = {
          |    response match {
          |      case r: ShowPetByIdResponse200 => Ok(r.content)
          |      case r: ShowPetByIdResponseDefault => implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode)).withEntity(r.content))
          |    }
          |  }
          |  def asHttp4sResponse(response: LogoutResponse): F[Response[F]] = {
          |    response match {
          |      case r: LogoutResponse405 => MethodNotAllowed(org.http4s.headers.Allow(r.allowMethods))
          |      case r: LogoutResponseDefault => implicitly[Applicative[F]].pure(Response(org.http4s.Status(r.httpCode)))
          |    }
          |  }
          |}
          |
          |case class RequestData[F[_], I](
          |  headerData: I,
          |  raw: Request[F]
          |)
          |
          |trait ServiceImpl[F[_], I] {
          |  import Responses._
          |
          |  def listPets(request: RequestData[F, I], limit: Option[Int], headers: Headers = Headers.empty): F[ListPetsResponse]
          |  def createPets(request: RequestData[F, I], body: Components.RequestBodies.CreatePetRequest, headers: Headers = Headers.empty): F[CreatePetsResponse]
          |  def updatePet(request: RequestData[F, I], petId: Int, body: Components.RequestBodies.UpdatePetRequest, headers: Headers = Headers.empty): F[UpdatePetResponse]
          |  def showPetById(request: RequestData[F, I], petId: Int, headers: Headers = Headers.empty): F[ShowPetByIdResponse]
          |  def logout(request: RequestData[F, I], headers: Headers = Headers.empty): F[LogoutResponse]
          |}
          |
          |@nowarn object ParameterMatchers {
          |  import org.http4s.dsl.impl.{QueryParamDecoderMatcher, OptionalQueryParamDecoderMatcher}
          |  import org.http4s.{QueryParameterValue, QueryParamDecoder}
          |
          |  abstract class ExplodingQueryParamDecoderMatcher[T: QueryParamDecoder](name: String) {
          |    def unapply(
          |      params: Map[String, collection.Seq[String]]
          |    ): Option[Vector[T]] = {
          |      params.get(name) match {
          |        case Some(values) =>
          |          values.toVector
          |            .traverse(s => QueryParamDecoder[T].decode(QueryParameterValue(s)))
          |            .toOption
          |        case None =>
          |          Some(Vector.empty)
          |      }
          |    }
          |  }
          |
          |  object ListPetsLimitQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Int]("limit")
          |}
          |
          |object Service {
          |  def makeRoutes[F[_]: Concurrent, I](
          |    impl: ServiceImpl[F, I],
          |    preProcessHeaders: Headers => F[Either[Response[F], I]],
          |    postProcessHeaders: Headers => Headers = identity
          |  ) = {
          |    def withPreProcessedHeaders(req: Request[F])(action: RequestData[F, I] => F[Response[F]]) = {
          |      for {
          |        preProcessResult <- preProcessHeaders(req.headers)
          |        result <- preProcessResult.fold(
          |          response => Concurrent[F].pure(response),
          |          data => action(RequestData(data, req))
          |        )
          |      } yield {
          |        result
          |      }
          |    }
          |
          |    val dsl = new Http4sDsl[F]{}
          |    val responseConverters = new ResponseConverters[F](dsl)
          |    import dsl._
          |    import responseConverters._
          |    import ParameterMatchers._
          |
          |    HttpRoutes.of[F] {
          |      case req @ GET -> Root / "pets" :? ListPetsLimitQueryParamMatcher(limit) => withPreProcessedHeaders(req) { requestData =>
          |        impl.listPets(requestData, limit)
          |          .flatMap(asHttp4sResponse)
          |          .map(response => response.withHeaders(postProcessHeaders(response.headers)))
          |      }
          |      case req @ POST -> Root / "pets" => withPreProcessedHeaders(req) { requestData =>
          |        for {
          |          body <- req.as[Components.RequestBodies.CreatePetRequest]
          |          resp <- impl.createPets(requestData, body)
          |          http4sResponse <- asHttp4sResponse(resp)
          |        } yield {
          |          http4sResponse.withHeaders(postProcessHeaders(http4sResponse.headers))
          |        }
          |      }
          |      case req @ PUT -> Root / "pets" / IntVar(petId) => withPreProcessedHeaders(req) { requestData =>
          |        for {
          |          body <- req.as[Components.RequestBodies.UpdatePetRequest]
          |          resp <- impl.updatePet(requestData, petId, body)
          |          http4sResponse <- asHttp4sResponse(resp)
          |        } yield {
          |          http4sResponse.withHeaders(postProcessHeaders(http4sResponse.headers))
          |        }
          |      }
          |      case req @ GET -> Root / "pets" / IntVar(petId) => withPreProcessedHeaders(req) { requestData =>
          |        impl.showPetById(requestData, petId)
          |          .flatMap(asHttp4sResponse)
          |          .map(response => response.withHeaders(postProcessHeaders(response.headers)))
          |      }
          |      case req @ POST -> Root / "logout" => withPreProcessedHeaders(req) { requestData =>
          |        impl.logout(requestData)
          |          .flatMap(asHttp4sResponse)
          |          .map(response => response.withHeaders(postProcessHeaders(response.headers)))
          |      }
          |    }
          |  }
          |}
          |""".stripMargin

      renderResult shouldBe expected
    }
  }
}

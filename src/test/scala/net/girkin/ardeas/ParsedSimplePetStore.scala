package net.girkin.ardeas

import net.girkin.ardeas.Model.Parameter
import net.girkin.ardeas.Model.Parameter.{PathParameter, QueryParameter}

object ParsedSimplePetStore {

  import net.girkin.ardeas.Model.*

  def api = Api(
    Vector(
      HttpOperation(
        Path(List(PathSegment.StringSegment("pets"))),
        HttpVerb.Get,
        Option("listPets"),
        Vector(
          QueryParameter("limit", Schema.StandardType("integer", None), false)
        ),
        Vector(
          Response(200, ResponseBody.Definition(Some(NamedSchemaRef("Pets")))),
          Response(Default, ResponseBody.Definition(Some(NamedSchemaRef("Error"))))
        ),
        None
      ),
      HttpOperation(
        Path(List(PathSegment.StringSegment("pets"))),
        HttpVerb.Post,
        Option("createPets"),
        Vector.empty,
        Vector(
          Response(201, ResponseBody.Definition(None)),
          Response(Default, ResponseBody.Definition(Some(NamedSchemaRef("Error"))))
        ),
        Some(RequestBody.NamedRef("CreatePetRequest"))
      ),
      HttpOperation(
        Path(List(PathSegment.StringSegment("pets"), PathSegment.TemplatedParameter("petId"))),
        HttpVerb.Put,
        Option("updatePet"),
        Vector(
          PathParameter("petId", Schema.StandardType("integer", Some("int32")))
        ),
        Vector(
          Response(200, ResponseBody.NamedRef("SinglePetResponse")),
          Response(Default, ResponseBody.Definition(Some(NamedSchemaRef("Error"))))
        ),
        Some(RequestBody.NamedRef("UpdatePetRequest"))
      ),
      HttpOperation(
        Path(List(PathSegment.StringSegment("pets"), PathSegment.TemplatedParameter("petId"))),
        HttpVerb.Get,
        Option("showPetById"),
        Vector(
          PathParameter("petId", Schema.StandardType("integer", Some("int32")))
        ),
        Vector(
          Response(200, ResponseBody.NamedRef("SinglePetResponse")),
          Response(Default, ResponseBody.Definition(Some(NamedSchemaRef("Error"))))
        ),
        None
      ),
      HttpOperation(
        Path(List(PathSegment.StringSegment("logout"))),
        HttpVerb.Post,
        Option("logout"),
        Vector.empty,
        Vector(
          Response(405, ResponseBody.Definition(None)),
          Response(Default, ResponseBody.Definition(None))
        ),
        None
      )
    ),
    Map(
      "Pet" -> Schema.makeObject(
        EntityField("id", Schema.StandardType("integer", Some("int32")), true),
        EntityField("name", Schema.StandardType("string", None), true),
        EntityField("tag", Schema.StandardType("string", None), false),
      ),
      "Pets" -> Schema.Array(NamedSchemaRef("Pet")),
      "Error" -> Schema.makeObject(
        EntityField("code", Schema.StandardType("integer", Some("int64")), true),
        EntityField("message", Schema.StandardType("string", None), true)
      )
    ),
    Map(
      "CreatePetRequest" -> RequestBody.Definition(
        true,
        Some(NamedSchemaRef("Pet"))
      ),
      "UpdatePetRequest" -> RequestBody.Definition(
        false,
        Some(NamedSchemaRef("Pet"))
      )
    ),
    Map(
      "SinglePetResponse" -> ResponseBody.Definition(Some(
        NamedSchemaRef("Pet")
      ))
    )
  )
}

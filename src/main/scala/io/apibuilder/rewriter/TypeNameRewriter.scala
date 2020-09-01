package apibuilder.rewriter

import io.apibuilder.commons.types.ApiBuilderType

object TypeNameRewriter {
  def changeName(apiBuilderType: ApiBuilderType, name: String): ApiBuilderType = {
    apiBuilderType match {
      case t: ApiBuilderType.Enum => ApiBuilderType.Enum(t.service, t.`enum`.copy(name = name))
      case t: ApiBuilderType.Model => ApiBuilderType.Model(t.service, t.model.copy(name = name))
      case t: ApiBuilderType.Union => ApiBuilderType.Union(t.service, t.union.copy(name = name))
    }
  }
}

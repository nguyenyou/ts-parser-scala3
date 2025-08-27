package io.github.nguyenyou.internal
package scalajs

import io.circe.{Decoder, Encoder}
import io.github.nguyenyou.internal.stringUtils.quote

sealed trait Dep {
  def org:     String
  def version: String

  def for3Use2_13(is3: Boolean): Dep =
    if (is3) Dep.For3Use2_13(this) else this

  def asSbt: String =
    this match {
      case Dep.Mangled(_, dep) =>
        dep.asSbt
      case Dep.Java(_, artifact, _) =>
        s"${quote(org)} % ${quote(artifact)} % ${quote(version)}"
      case Dep.Scala(_, artifact, _) =>
        s"${quote(org)} %% ${quote(artifact)} % ${quote(version)}"
      case Dep.ScalaJs(_, artifact, _) =>
        s"${quote(org)} %%% ${quote(artifact)} % ${quote(version)}"
      case Dep.ScalaFullVersion(_, artifact, _) =>
        s"${quote(org)} % ${quote(artifact)} % ${quote(version)} cross CrossVersion.Full()"
      case Dep.For3Use2_13(dep) =>
        s"""(${dep.asSbt}).cross(CrossVersion.for3Use2_13)"""
    }

  def concrete(versions: Versions): Dep.Concrete =
    this match {
      case concrete: Dep.Concrete => concrete
      case Dep.Scala(_, artifact, _) =>
        Dep.Mangled(s"${artifact}_${versions.scala.binVersion}", this)
      case Dep.ScalaJs(_, artifact, _) =>
        Dep.Mangled(s"${artifact}_sjs${versions.scalaJs.scalaJsBinVersion}_${versions.scala.binVersion}", this)
      case Dep.ScalaFullVersion(_, artifact, _) =>
        Dep.Mangled(s"${artifact}_${versions.scala.scalaVersion}", this)
      case Dep.For3Use2_13(dep) =>
        dep.concrete(versions.copy(scala = Versions.Scala213))
    }
}

object Dep {
  sealed trait Concrete extends Dep {
    def mangledArtifact: String

    def asMangledSbt: String =
      s"${quote(org)} % ${quote(mangledArtifact)} % ${quote(version)}"

    def asIvy(config: String = "compile->default(compile)") =
      s"""
      <dependency org={org} name={mangledArtifact} rev={version} conf={config}/>
      """

    // format: off
    def asMaven =
      s"""
      <dependency>
        <groupId>{org}</groupId>
        <artifactId>{mangledArtifact}</artifactId>
        <version>{version}</version>
      </dependency>
      """

    def asMavenTest =
      s"""
      <dependency>
        <groupId>{org}</groupId>
        <artifactId>{mangledArtifact}</artifactId>
        <version>{version}</version>
        <scope>test</scope>
      </dependency>
      """
    // format: on
  }
  object Concrete {
    implicit val encodes: Encoder[Concrete] = io.circe.generic.semiauto.deriveEncoder
    implicit val decodes: Decoder[Concrete] = io.circe.generic.semiauto.deriveDecoder
  }

  case class Java(org: String, name: String, version: String) extends Concrete {
    override def mangledArtifact: String = name
  }
  case class For3Use2_13(dep: Dep) extends Dep {
    override def org:     String = dep.org
    override def version: String = dep.version
  }
  case class Scala(org:            String, name: String, version: String) extends Dep
  case class ScalaFullVersion(org: String, name: String, version: String) extends Dep
  case class ScalaJs(org:          String, name: String, version: String) extends Dep

  case class Mangled(mangledArtifact: String, dep: Dep) extends Concrete {
    override def org:     String = dep.org
    override def version: String = dep.version
  }

  implicit val decodes: Decoder[Dep] = io.circe.generic.semiauto.deriveDecoder
  implicit val encodes: Encoder[Dep] = io.circe.generic.semiauto.deriveEncoder
}

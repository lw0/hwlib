package hwlib.xilinx.tools

import scala.collection.mutable

import spinal.core._
import spinal.core.internals._
import spinal.lib._

import hwlib.xilinx.board.Board
import hwlib.{WriteArtifact, Expand}


sealed trait ProjectArtifactKind
object ProjectSource extends ProjectArtifactKind
object ProjectConstr extends ProjectArtifactKind
object ProjectFile extends ProjectArtifactKind


case class ProjectScript(tcl : String, comment : String = "") extends SpinalTag
case class ProjectToplevel(name : String) extends SpinalTag
case class ProjectArtifact(name : String, contents : String, kind : ProjectArtifactKind = ProjectFile) extends SpinalTag


class ProjectGenerator[T <: Component](val report : SpinalReport[T], val board : Board, val name : Option[String] = None) {

  val portInfo = mutable.Map[(String, Option[Int]), Map[String, String]]()
  val clockInfo = mutable.Map[(String, Option[Int]), Clock]()
  val toplevelName = report.toplevel.getTag(classOf[ProjectToplevel]).map(_.name).getOrElse(report.toplevel.definitionName)
  val projectName = name.getOrElse(report.toplevel.definitionName)

  def generate() = {
    var buf = new StringBuilder()

    val constrFiles = mutable.ArrayBuffer[String]()
    val sourceFiles = mutable.ArrayBuffer[String]()
    val otherFiles  = mutable.ArrayBuffer[String]()

    //sourceFiles ++= report.rtlSourcesPaths.map(_.replace("//", "/"))

    report.globalData.phaseContext.walkComponents{ comp =>
      comp.foreachTag {
        case tag : ProjectArtifact =>
          val path = WriteArtifact(report.globalData, s"{{outDir}}/${tag.name}", tag.contents)
          tag.kind match {
            case ProjectSource => sourceFiles += path
            case ProjectConstr => constrFiles += path
            case ProjectFile => otherFiles += path
          }
        case _ => ()
      }
    }

    buf ++= s"set project_name ${projectName}_proj\n"
    buf ++= s"set project_dir [lindex $$argv 0]\n"
    buf ++= s"create_project -part ${board.part} -force $$project_name $$project_dir\n"
    if (board.board.isDefined) {
      buf ++= s"set_property board_part ${board.board.get} [current_project]\n"
    }

    buf ++= s"\n### Setup Sources\n"
    buf ++= s"set SourceList [open {{outDir}}/{{topName}}.lst]\n"
    buf ++= s"add_files -fileset [current_fileset] [split [read -nonewline $$SourceList] \"\\n\"]\n"
    buf ++= s"close $$SourceList\n"
    if (sourceFiles.nonEmpty) {
      buf ++= s"add_files -fileset [current_fileset] [list \\\n  ${sourceFiles.mkString("\\\n  ")}]\n"
    }
    if (constrFiles.nonEmpty) {
      buf ++= s"add_files -fileset [current_fileset -constrset] [list \\\n  ${constrFiles.mkString("\\\n  ")}]\n"
    }

    report.globalData.phaseContext.walkComponents{ comp =>
      comp.foreachTag{
        case ipTag : ProjectScript => {
          buf ++= s"\n### Script for \"${comp.definitionName}\"\n"
          buf ++= s"# ${ipTag.comment}\n"
          buf ++= Expand(ipTag.tcl)(Map("compName" -> comp.definitionName).get(_))
        }
        case _ => ()
      }
    }

    buf ++= s"\n### Setup Toplevel\n"
    buf ++= s"update_compile_order -fileset [current_fileset]\n"
    buf ++= s"set_property top ${toplevelName} [current_fileset]\n"

    WriteArtifact(report.globalData, "{{outDir}}/create_project.tcl", buf.toString)

    buf.clear()
    buf ++= s"set project_name ${projectName}_proj\n"
    buf ++= s"set project_dir [lindex $$argv 0]\n"
    buf ++= s"open_project $$project_dir/$$project_name\n"
    buf ++= s"auto_detect_xpm\n"
    // TODO-lw make configurable
    buf ++= s"set_property strategy Performance_Auto_1 [get_runs impl_1]\n"
    buf ++= s"launch_runs impl_1 -to_step write_bitstream -jobs 4\n"
    buf ++= s"wait_on_runs -quiet [current_run]\n"
    buf ++= s"write_hw_platform -fixed -include_bit -force -file $$project_dir/${projectName}.xsa\n"
    buf ++= s"file copy -force $$project_dir/$$project_name.runs/impl_1/${toplevelName}.bit $$project_dir/${projectName}.bit\n"
    WriteArtifact(report.globalData, "{{outDir}}/build_project.tcl", buf.toString)

    // val config = report.globalData.config
    // TODO-lw use config.mode, or stick to Verilog due to MIG?
    // buf ++= s"set_property target_language Verilog [current_project]"

    // buf ++= s"Toplevel = ${report.toplevel.definitionName}\n"
    // buf ++= s"Files = ${fileString}\n"
    // buf ++= s"Xdc = ${xdcFile}"
    // TODO-lw create project

  }


}


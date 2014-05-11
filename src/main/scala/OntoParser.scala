import java.io.{InputStreamReader, FileInputStream}
import scala.util.parsing.input.StreamReader

/**
 * Created by Keenon on 5/10/14.
 */
object OntoParser {
  def parse(path : String) {
    val parseFile = FileUtils.file(path+".parse")
    val propFile = FileUtils.file(path+".parse")
    if (parseFile.exists && propFile.exists) {
      val parseSr = StreamReader(new InputStreamReader(new FileInputStream(parseFile)))
      val trees : List[PTBNode] = PTBParser.parseAll(PTBParser.trees,parseSr).get
      val propSr = StreamReader(new InputStreamReader(new FileInputStream(propFile)))
      val props : List[NTuple] = PropParser.parseAll(PropParser.props,propSr).get
      println("First prop, first sense: "+props(0).senses(0))
    }
  }
}

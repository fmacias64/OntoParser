import com.mongodb.casbah.MongoCollection
import java.io.{InputStreamReader, FileInputStream}
import scala.util.parsing.input.StreamReader

/**
 * Created by Keenon on 5/10/14.
 */
object OntoParser {

  def printNTupleArg(arg : NTupleArg, tree : PTBNode) {
    println(arg.rel+":"+PTBTreeUtils.flatten(PTBTreeUtils.specifiedSubtree(tree,arg.mapping(0))))
  }

  def insert(path : String, docbank : MongoCollection) {

    val subpath = path.split("annotations/")(1)
    val documentIndex = subpath.substring(subpath.indexOf("/")+1)
    println("Parsing "+documentIndex)

    val parseFile = FileUtils.file(path+".parse")
    val propFile = FileUtils.file(path+".prop")
    val corefFile = FileUtils.file(path+".coref")
    val nameFile = FileUtils.file(path+".name")

    if (parseFile.exists && propFile.exists && corefFile.exists && nameFile.exists) {

      val parseSr = StreamReader(new InputStreamReader(new FileInputStream(parseFile)))
      val propSr = StreamReader(new InputStreamReader(new FileInputStream(propFile)))

      val trees : List[PTBNode] = PTBParser.parseAll(PTBParser.trees,parseSr).get
      val props : List[NTuple] = PropParser.parseAll(PropParser.props,propSr).get
      val corefChains : List[TagSequence] = CorefParser.parse(path+".coref",trees)
      val names : List[TagSequence] = NameParser.parse(path+".name",trees)

      println("CorefChains: "+corefChains.length)
      println(corefChains(0))
      println(corefChains(1))

      println("Names: "+names.length)
      println(names(0))
      println(PTBTreeUtils.concatTerminals(PTBTreeUtils.getAllInterveningTokens(trees,names(0).startSentence,names(0).startIndex,names(0).endSentence,names(0).endIndex)))

      println("Trees: "+trees.length)

      //MongoInterface.createDocument(docbank,documentIndex,trees,props,corefChains ++ names)
    }
  }
}

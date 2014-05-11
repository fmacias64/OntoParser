import java.io.{InputStreamReader, FileInputStream}
import scala.util.parsing.input.StreamReader

/**
 * Created by Keenon on 5/10/14.
 */
object OntoParser {

  def printNTupleArg(arg : NTupleArg, tree : PTBNode) {
    println(arg.rel+":"+PTBTreeUtils.flatten(PTBTreeUtils.specifiedSubtree(tree,arg.mapping(0))))
  }

  def parse(path : String) {
    val subpath = path.split("annotations/")(1)
    val index = subpath.substring(subpath.indexOf("/")+1)
    println("Trying to parse "+index)
    val parseFile = FileUtils.file(path+".parse")
    val propFile = FileUtils.file(path+".prop")
    if (parseFile.exists && propFile.exists) {
      println("Both exist")
      val parseSr = StreamReader(new InputStreamReader(new FileInputStream(parseFile)))
      val trees : List[PTBNode] = PTBParser.parseAll(PTBParser.trees,parseSr).get
      val propSr = StreamReader(new InputStreamReader(new FileInputStream(propFile)))
      val props : List[NTuple] = PropParser.parseAll(PropParser.props,propSr).get
      val corefChains : List[TagSequence[CorefData]] = CorefParser.parse(path+".coref")
      val names : List[TagSequence[NameData]] = NameParser.parse(path+".name")

      println("CorefChains: "+corefChains.length)
      println(corefChains(0))
      println(corefChains(1))

      println("Names: "+names.length)
      println(names(0))

      // Print out all the props

      /*props.foreach(nt => {
        val tree = trees(nt.sentenceNum)
        println("Senses:")
        nt.senses.foreach(s => println(s))
        println("Arguments:")
        nt.args.foreach(a => printNTupleArg(a,tree))
      })*/
    }
  }
}

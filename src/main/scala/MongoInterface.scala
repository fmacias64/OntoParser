import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoCollection, MongoDB, MongoClient}
import scala.collection.parallel.mutable
import scala.collection.parallel.mutable.{ParSet, ParHashMap}
import scala.collection.mutable._

/**
 * Created by Keenon on 5/10/14.
 */
object MongoInterface {

  def clearDatabase(docbank : MongoCollection) {
    docbank.remove(MongoDBObject())
  }

  def nTupleArgToMongoObject(nTupleArg : NTupleArg, sentence : PTBNode, idMap : ParHashMap[PTBNode,Int]) = {
    MongoDBObject("rel" -> nTupleArg.rel,
      "raw_mappings" -> nTupleArg.mapping.map(ia => MongoDBObject("index"->ia.index,"ancestors"->ia.ancestor)),
      "mappings" -> nTupleArg.mapping.map(ia => idMap(PTBTreeUtils.specifiedSubtree(sentence,ia))))
  }

  def nTupleToMongoObject(nTuple : NTuple, trees : List[PTBNode], idMap : ParHashMap[PTBNode,Int]) = {
    MongoDBObject("sentence_num" -> nTuple.sentenceNum, "senses" -> nTuple.senses, "arguments" -> nTuple.args.map(nTupleArgToMongoObject(_,trees(nTuple.sentenceNum),idMap)))
  }

  def tagDataToMongoObject(tagData : TagData) = {
    tagData match {
      case corefData : CorefData => MongoDBObject("data_type" -> "coref", "chain_id" -> corefData.id, "ref_type" -> corefData.refType)
      case nameData : NameData => MongoDBObject("data_type" -> "name", "ner_type" -> nameData.nameType)
    }
  }

  def tagSequenceToMongoObject(tagSequence : TagSequence, trees: List[PTBNode], idMap : ParHashMap[PTBNode,Int]) = {
    MongoDBObject("start_sentence" -> tagSequence.startSentence,
      "start_index" -> tagSequence.startIndex,
      "end_sentence" -> tagSequence.endSentence,
      "end_index" -> tagSequence.endIndex,
      "tag_data" -> tagDataToMongoObject(tagSequence.data),
      "ids" -> PTBTreeUtils.getAllInterveningTokens(trees,tagSequence.startSentence,tagSequence.startIndex,tagSequence.endSentence,tagSequence.endIndex).map(node => idMap(node)))
  }

  case class PTBWrapper(node: PTBNode, id: Int, left: Int, right: Int, ancestors: List[Int], children: List[Int], sentence: Int)

  def recursiveWrapTree(tree : PTBNode, idCounter : Int, idMap : ParHashMap[PTBNode,Int], ancestors: List[Int], wrappers : ParSet[PTBWrapper], sentence: Int) : Int = {
    idMap.put(tree,idCounter)
    tree match {
      case nonTerminal : PTBNonTerminal => {
        val newIdCounter = nonTerminal.childen.foldLeft(idCounter+1)((id,child) => recursiveWrapTree(child,id,idMap,idCounter :: ancestors, wrappers, sentence))
        wrappers += PTBWrapper(tree, idCounter, idCounter, newIdCounter, ancestors, nonTerminal.childen.map(child => idMap(child)), sentence)
        newIdCounter+1
      }
      case terminal : PTBTerminal => {
        wrappers += PTBWrapper(tree, idCounter, idCounter, idCounter+1, ancestors, List(), sentence)
        idCounter+2
      }
    }
  }

  def wrapTree(tree : PTBNode, startId : Int, idMap : ParHashMap[PTBNode,Int], wrappers : ParSet[PTBWrapper], sentence : Int) : Int = {
    List[PTBWrapper]()
    recursiveWrapTree(tree,startId,idMap,List(),wrappers, sentence)
  }

  def ptbWrapperToMongoDBObject(ptbWrapper : PTBWrapper) = {
    MongoDBObject("tag" -> ptbWrapper.node.l,
    "terminal" -> (ptbWrapper.node match {
      case terminal : PTBTerminal => terminal.token
      case _ => ""
    }),
    "flattened" -> PTBTreeUtils.flatten(ptbWrapper.node),
    "clean_flattened" -> PTBTreeUtils.cleanFlatten(ptbWrapper.node),
    "ptb_notation" -> ptbWrapper.node.toString,
    "children" -> ptbWrapper.children,
    "ancestors" -> ptbWrapper.ancestors,
    "sentence_num" -> ptbWrapper.sentence,
    "id" -> ptbWrapper.id,
    "left" -> ptbWrapper.left,
    "right" -> ptbWrapper.right)
  }

  def createDocument(docbank : MongoCollection, indexName : String, trees : List[PTBNode], nTuples : List[NTuple], tagSequences : List[TagSequence]) {

    val idMap : ParHashMap[PTBNode,Int] = ParHashMap()
    val wrappers : ParSet[PTBWrapper] = ParSet()
    trees.foldLeft(0)((id,node) => wrapTree(node, id, idMap, wrappers, trees.indexOf(node)))

    docbank.insert(MongoDBObject("index_name" -> indexName,
      "subtrees" -> wrappers.seq.map(ptbWrapperToMongoDBObject),
      "ntuples" -> nTuples.map(nTupleToMongoObject(_,trees,idMap)),
      "tag_sequences" -> tagSequences.map(tagSequenceToMongoObject(_,trees,idMap))))
  }

}

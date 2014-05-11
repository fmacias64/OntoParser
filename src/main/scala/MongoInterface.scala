import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.{MongoCollection, MongoDB, MongoClient}

/**
 * Created by Keenon on 5/10/14.
 */
object MongoInterface {

  def clearDatabase(docbank : MongoCollection) {
    docbank.remove(MongoDBObject())
  }

  def createDocument(docbank : MongoCollection, indexName : String, trees : List[PTBNode], nTuples : List[NTuple], tagSequences : List[TagSequence]) {
    docbank.insert(MongoDBObject("index_name" -> indexName))
  }

}

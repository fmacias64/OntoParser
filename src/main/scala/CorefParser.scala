import scala.xml.Elem

case class CorefData(id: String, refType: String) extends TagData

/**
 * Created by Keenon on 5/10/14.
 */
object CorefParser extends SequenceParser {
  override def dataLabel: String = "COREF"
  override def getData(node: Elem): TagData = CorefData((node \ "@ID").toString,(node \ "@TYPE").toString)
}

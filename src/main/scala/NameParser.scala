import scala.xml.Elem

case class NameData(nameType: String) extends TagData

/**
 * Created by Keenon on 5/10/14.
 */
object NameParser extends SequenceParser {
  override def dataLabel: String = "ENAMEX"
  override def getData(node: Elem): TagData = NameData((node \ "@TYPE").toString)
}

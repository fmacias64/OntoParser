import scala.xml.Elem

case class NameData(nameType: String) extends TagData

/**
 * Created by Keenon on 5/10/14.
 */
object NameParser extends SequenceParser[NameData] {
  override def dataLabel: String = "ENAMEX"
  override def getData(node: Elem): NameData = NameData((node \ "@TYPE").toString)
}

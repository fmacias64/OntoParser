import scala.util.parsing.combinator.RegexParsers

/**
 * Created by Keenon on 5/10/14.

 Files: *.prop
 Some examples:

nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 0 34 gold fail-v fail.01 ----- 34:0-rel 32:1-ARG1 33:1-ARGM-ADV 35:2-ARG2
nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 0 37 gold honor-v honor.01 ----- 37:0-rel 35:0*32:1-ARG0 38:1-ARG1
nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 1 17 gold remain-v remain.01 ----- 17:0-rel 0:1-ARGM-DIS 2:2-ARG1 16:0-ARGM-MOD 18:1-ARG3 26:1-ARGM-CAU
nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 1 39 gold announce-v announce.01 ----- 39:0-rel 34:2-ARG0 40:1-ARG1

 */

case class NTuple(senses: List[String], args: List[NTupleArg])
case class NTupleArg(rel: String, mapping: List[(Integer,Integer)])

object PropParser extends RegexParsers {
  def props: Parser[List[NTuple]] = prop.+
  def prop: Parser[NTuple] = "[^ ]+".r ~ "[0-9]+".r ~ "[0-9]+".r ~ "gold" ~ "[^ ]+".r.+ ~ "-----" ~ args ^^ {
    case source~sentenceNum~rootWord~"gold"~senses~"-----"~args => NTuple(senses, args)
  }
  def args: Parser[List[NTupleArg]] = arg.+
  def arg: Parser[NTupleArg] = ranges~"-"~"[^ ]+".r ^^ {
    case ranges~"-"~rel => NTupleArg(rel,ranges)
  }
  // Lists of ranges are strung together when we have null elements inserted into the parse
  def ranges: Parser[List[(Integer,Integer)]] = (range<~"*".?).+
  // First number is start word, second number is how many steps up the tree to take before you take the whole lot
  def range: Parser[(Integer,Integer)] = "[0-9]+:[0-9]+".r ^^ {
    s =>
      (Integer.parseInt(s.split(":")(0)),Integer.parseInt(s.split(":")(1)))
  }
}

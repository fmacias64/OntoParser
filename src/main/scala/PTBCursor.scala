/**
 * Created by Keenon on 5/11/14.
 */

case class SentenceIndex(sentence: Int, index: Int)

class PTBCursor(trees: List[PTBNode]) {
  var pos : SentenceIndex = SentenceIndex(0,0)
  var currentSentenceTokens : List[String] = getCurrentSentenceTokens

  def getCurrentSentenceTokens : List[String] = if (pos.sentence < trees.length) PTBTreeUtils.getAllSubtrees(List(trees(pos.sentence)),false).map(t => t match { case m : PTBTerminal => m.token }) else List()

  def incrementIndex() {
    pos = SentenceIndex(pos.sentence,pos.index+1)
  }

  def consumeLine() {
    if (pos.index == currentSentenceTokens.length) {
      pos = SentenceIndex(pos.sentence+1,0)
      currentSentenceTokens = getCurrentSentenceTokens
    }
  }

  def consumeToken(token : String) {
    if (token.contains("\n")) {
      var subtoken = token
      while (subtoken.contains("\n")) {
        subtoken.substring(0,subtoken.indexOf("\n")).split(" ").filter(!_.contains("\n")).map(consumeToken)
        subtoken = subtoken.substring(subtoken.indexOf("\n")+1)
        consumeLine()
      }
      subtoken.split(" ").map(st => {
        consumeToken(st)
      })
    }
    else if (token.trim.length == 0) {
      // Ignore it
    }
    else if (pos.index < currentSentenceTokens.length) {
      var expectToken = currentSentenceTokens(pos.index)
      while (expectToken != token && (expectToken.startsWith("*") || expectToken.startsWith("0") || expectToken.equals("\""))) {
        incrementIndex()
        expectToken = currentSentenceTokens(pos.index)
      }
      if (expectToken != token) System.err.println("Cursor error! Sentence: "+pos.sentence+" index: "+pos.index+" expecting: "+expectToken+" got: "+token)

      incrementIndex()
    }
  }
}

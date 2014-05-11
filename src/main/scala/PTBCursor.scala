/**
 * Created by Keenon on 5/11/14.
 */

case class SentenceIndex(sentence: Int, index: Int)

class PTBCursor(trees: List[PTBNode]) {
  var currentSentenceIndex : Int = 0
  var currentWithinSentenceIndex : Int = 0
  var currentSentenceTokens : List[String] = getCurrentSentenceTokens

  def index : SentenceIndex = SentenceIndex(currentSentenceIndex,currentWithinSentenceIndex)
  def getCurrentSentenceTokens : List[String] = PTBTreeUtils.getAllSubtrees(List(trees(currentSentenceIndex)),false).map(t => t match { case m : PTBTerminal => m.token })

  def incrementIndex() {
    currentWithinSentenceIndex += 1
    if (currentWithinSentenceIndex >= currentSentenceTokens.length) {
      currentSentenceIndex += 1
      currentWithinSentenceIndex = 0
      currentSentenceTokens = getCurrentSentenceTokens
    }
  }

  var lastExpect : String = ""
  var lastConsume : String = ""

  def consumeToken(token : String) {
    var expectToken = currentSentenceTokens(currentWithinSentenceIndex)
    while (expectToken != token && (expectToken.startsWith("*") || expectToken.startsWith("0"))) {
      incrementIndex()
      expectToken = currentSentenceTokens(currentWithinSentenceIndex)
    }
    if (expectToken != token) println("Cursor error! Sentence: "+currentSentenceIndex+" index: "+currentWithinSentenceIndex+" expecting: "+expectToken+" got: "+token+"\nlast expect: "+lastExpect+" last got: "+lastConsume)

    lastExpect = expectToken
    lastConsume = token

    incrementIndex()
  }
}

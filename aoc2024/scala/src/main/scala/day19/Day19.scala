package day19

import scala.io.Source

def pt1(patterns: List[String], designs: List[String]): Int =
  val sortedPatterns = patterns.sortBy(_.length).reverse
  println(s"patterns: ${sortedPatterns.length}")
  val filteredPatterns = filterPatterns(sortedPatterns)
  println(s"filtered: ${filteredPatterns.length}")
  designs.count(d => isPossible(filteredPatterns)(d))

def filterPatterns(patterns: List[String]): List[String] =
  val singleDigitPatterns = patterns.filter(_.length == 1).map(_.head).toSet
  patterns.filterNot(p => p.length > 1 && p.forall(singleDigitPatterns.contains))

def isPossible(patterns: List[String])(design: String): Boolean =
  if design.isEmpty then true
  else
    val allMatches = patterns.filter(design.startsWith)
    if allMatches.isEmpty then
      false
    else
      allMatches.exists(m => isPossible(patterns)(design.drop(m.length)))

def parseInput(strings: List[String]): (List[String], List[String]) =
  (strings.head.split(",").map(_.trim).toList, strings.last.split("\n").toList)

@main
def main(): Unit =
  val input = Source.fromResource("day19.txt").mkString.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val (patterns, designs) = parseInput(input)
  println(s"pt1: ${pt1(patterns, designs)}")
  println(s"pt2: ${}")

// 335 - not the right answer

val example =
  """
    |r, wr, b, g, bwu, rb, gb, br
    |
    |brwrr
    |bggr
    |gbbr
    |rrbgbr
    |ubwu
    |bwurrg
    |brgr
    |bbrgwb
    |""".stripMargin.split("\n\n").map(_.trim).filterNot(_.isBlank).toList

val example2 =
  """
    |uurr, uugbw, rg, wugbbb, uru, ububw, uu, uwr, rgrgb, rurru, bbub, rww, urggbbb, rbur, grur, grw, guru, rgu, bwbw, ru, grrbbur, urr, bwbbbg, brrbr, wgw, rurbrbrr, wuu, wggw, wuuuru, wrg, ugww, gggrrg, gwruru, rrw, rbb, wgwrw, wug, bwurwbu, uurw, gbb, gbrbg, gwubwbrr, rbggr, bwgwg, uwg, guwbbw, rguwb, bgbu, grr, ubw, ggwbu, bburgr, u, urgb, rrb, wwbrru, ruruw, gr, uruwwur, brw, gwgu, rgbg, gug, rrbb, uuubub, rwuu, bru, uruuwbr, gubuuw, ruwu, rurbb, bu, bwbrwggr, wbw, ubwuw, buwugb, bur, uwwu, urb, rbrb, gu, wguwr, urrgw, ur, uwgbg, bbbugr, wuww, uggr, bbgwubb, uwgr, bubgb, bubu, bbr, rwrrb, gbuu, bgwbg, rrru, wgwrr, bug, bubrwb, burubu, bbwr, gwgg, uggubg, rgrub, ggbrrgu, wrbgru, gwr, uwgu, wuwub, gg, rrrbwrr, guw, gruu, rwrw, rbbgrb, rbugbr, buugbu, ggrrbuww, wgru, uur, wuwwr, bb, bgbg, gwurrwu, rbu, gwurg, rrrugwub, rgr, rru, wrgbgr, grg, rbrwrg, wrwgwwb, bugw, rwr, ugug, rwgg, ruw, ubg, brbw, bbrgrb, uww, uwwur, uubbr, wbrw, ggu, gbw, w, bggu, wwbrb, rbg, gbgubgu, bbbu, bgbur, uurur, uwuwu, uuwbw, wwgruub, wgwwwwr, buwr, gbuwg, wur, gbgrwb, buu, gbr, ggw, bwbbgbww, bruug, rruruu, bwgw, gww, ruwugggr, burb, brug, rbwurug, uuggwru, bbu, gbrgbuu, ruggwbw, wgur, gggru, rur, wuwgr, wwbwubr, guurwu, rwrrbub, wr, rugbgu, uugwwb, brr, gbwub, bub, rwrubw, uguwbggb, bruubbbw, wbrgu, ubb, buugr, bbwuww, wwugrw, wwb, bwgrwuw, wgg, bgwu, rguubw, uwrguw, uuwgu, ruwwb, ggg, urwub, rwu, bubuu, wubb, uwug, bg, gggbubg, rwrrwgu, wbr, brbu, bbuu, uruwur, uwwwrwbr, bwg, uub, grbrgw, bbug, wrgggrwg, gggurb, wgb, bbbbrbww, ggwb, rgw, bubuwgu, uwu, ubrurbu, grb, wrrw, wugw, brgwrugr, wrw, wgu, grgr, uuurw, ugrgrwr, www, bbg, ugg, gbu, uwrg, wbu, brb, rgb, rbrgbwu, ggb, wbwgr, grguwurr, brbrbr, ugw, gburg, gbbw, rrrbr, wwgg, rrr, bgu, wugbwgw, ruwuw, wwu, guug, gggub, gwbgw, wbwru, bwgru, rrugrw, uwuwrw, wbb, rr, gbuw, uwww, brg, urw, bgwb, wggggg, uwbbb, rwrr, rgwr, ugr, bgbggw, ubub, wrbggub, rrubr, ggrwr, wwur, wwuug, bbw, bbur, ruuwwrbr, ugrw, bwb, rrbwgw, bgbggwg, ubbw, uubwrg, rrwu, gbwb, bubg, brbggb, guugrb, wuwwru, rwrg, rgbrgr, uuw, ubrgwr, wwbrw, grbgg, urgg, bbrwu, rw, ubwwr, uugwg, ruubuw, wrrwrwuw, rbub, urg, buw, buuu, rbbwrr, rggwrw, urgugr, bwr, guu, wwuwug, rurb, bgb, rgbb, bgr, wuub, ubuwurw, buuwr, ggwuwgwb, gbug, ug, ruuru, rrg, rwuubww, wbg, buwb, ugrugb, gwb, wub, ggrbr, guwr, wwrwr, wgrw, bgw, ww, wuubgru, uw, bbwuurg, wuuwub, wrbuu, gw, bgrrgbb, wrbbgwwb, b, ruww, rbruu, bgg, uguww, wb, wgr, rubub, rbru, rrgb, ugwrww, gbg, wrgr, rbw, ugbuwww, gru, ruu, wugr, wgwg, wu, gwgbbug, wrbwrrgb, ugb, rwg, bgwburb, ggr, brggg, ruuuu, ugu, grbwu, ub, uwb, ubu, burwwrgr, wwg, gbwgu, wru, ggbwu, gwgubb, bbbww, rub, ubbbg, gwwb, rwugu, urggwbr, gwub, ubr, wrgu, gub, rb, wrb, rug, wuw, rubgrrg, bbbw, gbgrg, wbrggw, urbb, rbr, buuurg, uggu, bgrr, wrr, bubrg, urwu, wbbr, bww, gbbuug, rgg, bgbw, gb, uuwu, uuu, br, wgbg, rrbuurb, bwu, r, gur, rwug, uuwru, bbb, gugugw
    |
    |urbrrgubrurrgbrbggbgbgburuwbwgrguwurrwburgwgwgwg
    |""".stripMargin.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
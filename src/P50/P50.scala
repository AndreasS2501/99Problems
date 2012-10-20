package huffman
import scala.collection.mutable.HashMap

object P50 {

  class Knoten(val h: Int, val zeichen: Char) {
    def this(h: Int) = this(h, '1');
    private var kante: Knoten = null
    private var kantenWert: Int = -1
    def getKante() = kante
    def setKante(k: Knoten, kw: Int) = { kante = k; kantenWert = kw }
    def hasKante() = kante != null
    def isLeaf() = !zeichen.isDigit
    def getCode() = kantenWert
    override def toString() = h + " kante:" + nn(kante)
    def nn(k: Knoten) = if (k == null) "" else k.h
  }
  
  def findFreq(s:String):HashMap[Char,Int] = {
   val r=new HashMap[Char,Int]
   s.distinct.foreach(x=> r+= (x -> (s count(y => y==x)) ) )
   r
  }

  def transform(l: List[Knoten]): List[Knoten] = {
    val step = l filter (!_.hasKante) take 2
    if (step.size > 1) {
      val k = new Knoten(step(0).h + step(1).h)
      step(0).setKante(k, 0);
      step(1).setKante(k, 1);
      transform(((k :: l) sortWith (sortKnoten)))
    } else
      l
  }

  def getP(c: Knoten, fillMe: String): String = {
    if (c.getCode != -1) //Wurzel
      getP(c.getKante, fillMe + c.getCode.toString)
    else
      fillMe.reverse
  }

  def sortKnoten(k1:Knoten, k2:Knoten) = k1.h < k2.h
  
  def sortAlpha(a:String, b:String):Boolean = a.toCharArray.apply(0) < b.toCharArray.apply(0)  

  def main(args: Array[String]): Unit = {
    //val fr=findFreq("MISSIPPI")
    //println(fr)
    
    val fr=Map(('a' -> 45), ('b' -> 13), ('c' -> 12), ('d'-> 16), ('e'-> 9), ('f'-> 5))
    
    val list1=((for {b <- fr; fre =b._2; element=b._1 } yield new Knoten(fre,element)).toList) sortWith (sortKnoten)
    val list1a=transform(list1)
    val resAnzeige=(list1a filter (_.isLeaf) map(x => x.zeichen+":"+getP(x,"") ))
    println(resAnzeige.sortWith (sortAlpha))
  }

}
package aoc2023.d15

def solution(filename: String): Long =
    io.Source.fromFile(filename)
        .getLines()
        .map(_.split(","))
        .flatten
        .map(hash(_))
        .foldLeft(0L)(_ + _.toLong)
        
def solution2(filename: String): Long =
    io.Source.fromFile(filename)
        .getLines()
        .map(_.split(","))
        .flatten
        .map(Step(_))
        .foldLeft(Array.ofDim[Box](256))((boxes, step) => {
            val box = if (boxes(step.box()) == null) then
                Box(step.box())
            else
                boxes(step.box())

            val lense = Lense(step)
            step.op match {
                case "-" => box.remove(lense)
                case "=" => box.replaceOrAdd(lense)
            }
            boxes(step.box()) = box
            
            boxes
        })
        .filter(_ != null)
        .foldLeft(0L)((acc, box) => {
            acc + box.power()
        })

class Step(s: String) {
    val label = s.takeWhile(c => c != '-' && c != '=')
    var op = s.dropWhile(c => c != '-' && c != '=').take(1)
    val focal = s.dropWhile(c => c != '-' && c != '=').drop(1).toIntOption.getOrElse(0)

    def box(): Int = hash(label)

    override def toString(): String = 
        s"label: $label, op: $op, focal $focal, box: ${box()}"
}

class Lense(s: Step) {
    val label = s.label
    var focal = s.focal

    override def equals(x: Any): Boolean = 
        x match {
            case that: Lense => that.label == this.label && that.focal == this.focal
            case _ => false
        }
    
    override def toString(): String = 
        s"[$label $focal]"
}

class Box(n: Int) {
    val num = n
    var lenses = List[Lense]()

    def remove(v: Lense) =
        lenses = lenses.filter(_.label != v.label)

    def replaceOrAdd(v: Lense) =
        if (lenses.filter(_.label == v.label).length > 0) {
            lenses = lenses.map(x => 
                if (x.label == v.label) then 
                    v
                else 
                    x
            )} 
        else {
            lenses = lenses :+ v
        }

    def power(): Long = 
        lenses
            .zipWithIndex
            .foldLeft(0L)((acc, x) => {
                val (lense, idx) = x
                val pwr = (this.num + 1) * (idx + 1) * lense.focal
                // println(s"box: $num, lense: $lense, pwr: $pwr")
                acc + pwr
            })

    override def toString(): String =
        s"box: $num, lenses: $lenses"
}

def hash(s: String): Int = 
    s.toCharArray()
        .foldLeft(0)((acc, c) => {
            ((acc + c.toByte) * 17) % 256
        })
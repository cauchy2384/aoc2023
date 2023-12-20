package aoc2023.d20

import scala.annotation.tailrec
import scala.collection.mutable.Queue

def solution(filename: String): Long =
    val modules = parse(filename)

    @tailrec
    def simulate(q: Queue[Signal], lows: Long, highs: Long): (Long, Long, Boolean) =
        if q.isEmpty then
            (lows, highs, false)
        else
            val signal = q.dequeue()
            if signal.isFinal() then
                (lows, highs, true)
            else
                if modules.contains(signal.to) then
                    val module = modules(signal.to)
                    val outputs = module.process(signal)
                    q.enqueueAll(outputs)
                
                if signal.pulse then 
                    simulate(q, lows, highs + 1) 
                else
                    simulate(q, lows + 1, highs) 
    
    def push(lows: Long, highs: Long): (Long, Long, Boolean) =
        val q = Queue[Signal](Signal("button", "broadcaster", false))
        simulate(q, lows, highs)

    val (lows, highs, _) = 
        (1 to 1000)
        .foldLeft((0L, 0L, false))((acc, _) => {
            push(acc._1, acc._2)
        })
    lows * highs

def solution2(filename: String): Long =
    val modules = parse(filename)

    var nums = Map[String, Long]()

    @tailrec
    def simulate(q: Queue[Signal], target: List[String], it: Long): Long =
        if q.isEmpty then
            it
        else
            val signal = q.dequeue()
            if target.contains(signal.from) && signal.pulse then
                if !nums.contains(signal.from) then
                    nums = nums + (signal.from -> it)

            if modules.contains(signal.to) then
                val module = modules(signal.to)
                val outputs = module.process(signal)
                q.enqueueAll(outputs)
            
            simulate(q, target, it) 
    
    def push(target: List[String], it: Long): Long =
        val q = Queue[Signal](Signal("button", "broadcaster", false))
        simulate(q, target, it + 1) 
    
    @tailrec
    def search(target: List[String], it: Long): Boolean =
        val it2 = push(target, it)
        
        if it2 > 20000 then
            false
        else
            search(target, it2)

    // these 4 are connected to rx, so we need all of them to be true
    // at the same time
    search(List("sg", "lm", "dh", "db"), 0L)

    lcm(nums.values.map(BigInt(_)).toSeq).toLong

class Signal(val from: String, val to: String, val pulse: Boolean) {
    def isFinal(): Boolean = 
        to == "rx" && pulse == false

    override def toString: String = 
        s"$from -> $to, $pulse"
}

trait Module(name: String, mType: String, modules: List[String]) {
    var inputs: List[String] = List[String]()

    def addInput(module: String): Unit = 
        inputs = inputs :+ module

    def process(signal: Signal): List[Signal]

    def info(): String = 
        s"${mType} $name, inputs: ${inputs} outputs: ${modules}"
}

class Broadcaster(name: String, modules: List[String]) extends Module(name, "broadcaster", modules) {
    def process(signal: Signal): List[Signal] = 
        modules
            .map(m => Signal(name, m, signal.pulse))
}

class Output(name: String, modules: List[String]) extends Module(name, "output", modules) {
    def process(signal: Signal): List[Signal] = 
        List[Signal]()
}

class FlipFlop(name: String, modules: List[String]) extends Module(name, "flip-flop", modules) {
    var state: Boolean = false

    def process(signal: Signal): List[Signal] =
        if signal.pulse == false then
            state = !state
            modules.map(m => Signal(name, m, state))
        else
            List[Signal]()
}

class Conjunction(name: String, modules: List[String]) extends Module(name, "conjuction", modules) {
    var inputStates = Map[String, Boolean]()

    def process(signal: Signal): List[Signal] = 
        inputStates = inputStates + (signal.from -> signal.pulse)

        val state = inputs.foldLeft(true)((acc, input) => {
            val inputState = if inputStates.contains(input) then
                inputStates(input)
            else
                false
            
            acc && inputState
        })

        modules
            .map(m => Signal(name, m, !state))
}

def parse(filename: String): Map[String, Module] =
    var inputs = Map[String, List[String]]()

    var modules = io.Source.fromFile(filename)
        .getLines()
        .foldLeft(Map[String, Module]
            ("output" -> Output("output", List()))
        )
            ((modules, line) =>
            val splitted = line.split("->")
            var sname = splitted(0).trim()
            val links = splitted(1).split(",").map(_.trim()).toList
            val name = if sname.charAt(0) != 'b' then sname.drop(1) else sname
            val module = sname.charAt(0) match {
                case 'b' => Broadcaster(name, links)
                case '%' => FlipFlop(name, links)
                case '&' => Conjunction(name, links)
                case _ => throw new Exception("Unknown module type")
            }

            links.foreach(link => {
                if inputs.contains(link) then
                    inputs = inputs + (link -> (inputs(link) :+ name))
                else
                    inputs = inputs + (link -> List(name))
            })

            modules + (name -> module)
        )
    
    inputs.foreach((name, inputs) => {
        inputs.foreach(input => {
            if !modules.contains(name) then
                modules = modules + (name -> Output(name, List()))
            modules(name).addInput(input)
        })
    })
    
    modules

def lcm(list: Seq[BigInt]):BigInt=list.foldLeft(1:BigInt){
  (a, b) => b * a /
  Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
}
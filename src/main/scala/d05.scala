package aoc2023.d05

def solution1(filename: String): Long =
    val (almanac, _, _) = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .foldLeft((Almanac(), States.Seeds, 1))(parseLine)

    val result = almanac.seeds
        .map(seed => {
            States.values
                .toList
                .foldLeft(seed)((acc, state) => almanac.mapSeed(state, acc))
        })

    result.min

def solution2(filename: String): Long =
    val (almanac, _, _) = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .foldLeft((Almanac(), States.Seeds, 2))(parseLine)

    var res = almanac.seedRanges
        .flatMap(rng => {
            States.values
                .toList
                .foldLeft(List(rng))((acc, state) => {
                    acc.flatMap(rng => {
                        almanac.mapRange(state, rng)
                    })
                })
        })
    
    res
        .sortBy(_(0))
        .map(x => x(0))
        (0)

object States extends Enumeration {
  type State = Value

  val Seeds, S2S, S2F, F2W, W2L, L2T, T2H, H2L = Value
}

class Almanac {
    var seeds = List[Long]()
    var maps = List[Map[States.Value, List[Rng]]]()
    var seedRanges = List[Array[Long]]()

    def addSeed(seed: Long): Unit = {
        seeds = seed :: seeds
    }

    def addSeedRange(seed: Long, delta: Long): Unit = {
        seedRanges = Array(seed, seed + delta - 1) :: seedRanges
        seedRanges.sortBy(_(0))
    }

    def addMapRange(state: States.Value, rng: Rng): Unit = {
        val m = maps.find(_.contains(state))
        if m.isEmpty then
            maps = Map(state -> List(rng)) :: maps
        else
            maps = maps.map(m => {
                if m.contains(state) then
                    val rngs = m(state)
                    m + (state -> (rngs :+ rng))
                else
                    m
            })
    }

    def mapSeed(state: States.Value, seed: Long): Long =
        if state == States.Seeds then
            seed
        else
            val (s, found) = maps.find(_.contains(state))
                .get(state)
                .foldLeft((seed, false))((acc, rng) => {
                    val (seed, found) = acc
                    if found then
                        acc
                    else
                        if rng.src <= seed && seed <= (rng.src + rng.delta - 1) then
                            (rng.dest + (seed - rng.src), true)
                        else
                            acc
                })
            s

    def mapRange(state: States.Value, rng: Array[Long]): List[Array[Long]] = 
        if state == States.Seeds then
            List(rng)
        else
            val (left, res) = maps.find(_.contains(state))
                .get(state)
                .foldLeft((rng, List[Array[Long]]()))((acc, rng2) => {
                    var (rng, res) = acc
                    if rng == null then
                        acc
                    else
                        var (a, b) = (rng(0), rng(1))
                        if b < rng2.src then
                            acc
                        else if a > rng2.upper() then
                            acc
                        else if a < rng2.src && b <= rng2.upper() then
                            val b2 = rng2.src - 1
                            val x = rng2.convert(rng2.src)
                            val y = rng2.convert(b)
                            res = Array(x, y) :: res
                            (Array(a, b2), res)
                        else if rng2.src <= a && b <= rng2.upper() then
                            val x = rng2.convert(a)
                            val y = rng2.convert(b)
                            res = Array(x, y) :: res
                            (null, res)
                        else if a < rng2.src && b > rng2.upper() then
                            val x1 = a
                            val y1 = rng2.src - 1
                            val x2 = rng2.convert(rng2.src)
                            val y2 = rng2.convert(rng2.upper())
                            val a2 = rng2.upper() + 1
                            res = Array(x1, y1) :: Array(x2, y2) :: res
                            (Array(a2, b), res)
                        else
                            val x = rng2.convert(a)
                            val y = rng2.convert(rng2.upper())
                            res = Array(x, y) :: res
                            val a2 = rng2.upper() + 1
                            (Array(a2, b), res)
                })
            if left != null then
                left :: res
            else 
                res
    

    override def toString: String =
        s"Almanac seeds: $seeds, maps: $maps"
}

class Rng(_dest: Long, _src: Long, _delta: Long) {
    def dest = _dest
    def src = _src
    def delta = _delta

    def upper(): Long = src + delta - 1

    def convert(seed: Long): Long =
        dest + (seed - src)

    override def toString: String =
        s"Range $dest, $src, $delta"    
}

def parseLine(acc: (Almanac, States.Value, Int), line: String): (Almanac, States.Value, Int) =
    var (almanac, state, part) = acc
    line match {
        case "" => // skip
        case s"seeds: $seeds" => 
            state = States.Seeds
            if part == 1 then
                seeds.split(" ")
                    .map(_.toLong)
                    .foreach(x => almanac.addSeed(x))
            else 
                seeds.split(" ")
                    .map(_.toLong)
                    .grouped(2)
                    .foreach(x => almanac.addSeedRange(x(0), x(1)))
        case "seed-to-soil map:" =>
            state = States.S2S
        case "soil-to-fertilizer map:" =>
            state = States.S2F
        case "fertilizer-to-water map:" =>
            state = States.F2W
        case "water-to-light map:" =>
            state = States.W2L
        case "light-to-temperature map:" =>
            state = States.L2T
        case "temperature-to-humidity map:" =>
            state = States.T2H
        case "humidity-to-location map:" =>
            state = States.H2L
        case _ =>
            val nums = line.split(" ").map(_.toLong)
            almanac.addMapRange(state, Rng(nums(0), nums(1), nums(2)))
    }

    (almanac, state, part)
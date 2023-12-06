package aoc2023.d06

def solution1(filename: String): Int =
    val parsed = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseLine)
        .toArray

    val times = parsed(0)
    val distances = parsed(1)

    val n = times.length
    Range(0, n)
        .toStream
        .map(i => {
            val time = times(i)
            val distance = distances(i)
            Range(0, time + 1)
                .toStream
                .map(j => {
                    val speed = j
                    val travelTime = time - j
                    speed * travelTime 
                })
                .filter(_ > distance)
                .foldLeft(0)((x, _) => x + 1)
        })
        .foldLeft(1)(_ * _)


def parseLine(line: String): Array[Int] = 
    val splitted = line.split(":")
    
    splitted(1)
        .trim()
        .split(" ")
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.toInt)


def solution2(filename: String): Int =
    val parsed = io.Source.fromFile(filename)
        .getLines()
        .toStream
        .map(parseLine2)
        .toArray
    
    val time = parsed(0)
    val distance = parsed(1)

    val l = search(time, distance, false)
    val r = search(time, distance, true)

    (r - l + 1).toInt

def search(time: math.BigInt, target: math.BigInt, rev: Boolean): math.BigInt =
    var i: math.BigInt = 0
    if rev then i = time else i = 0
    
    while dist(i, time - i) <= target do
        if rev then i -= 1 else i += 1
    
    i
    

def dist(speed: math.BigInt, time: math.BigInt): math.BigInt =
    speed * time

def parseLine2(line: String): math.BigInt = 
    val splitted = line.split(":")
    
    val s = splitted(1)
        .trim()
        .replaceAll(" ", "")

    math.BigInt(s)
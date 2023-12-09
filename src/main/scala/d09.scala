package aoc2023.d09

def solution1(filename: String): Int =
    solution(filename, false)

def solution2(filename: String): Int =
    solution(filename, true)

def solution(filename: String, left: Boolean): Int =
    io.Source.fromFile(filename)
        .getLines()
        .toStream
        .foldLeft(0)((acc, line) => {
            val nums = line.split(" ")
                .map(_.toInt)
                .toList
            val next = predict(nums, left)
            acc + next
        })

def predict(nums: List[Int], left: Boolean): Int = 
    if nums.foldLeft(0)(_ + _) == 0 then
        0
    else
        val reduced = Range(0, nums.length - 1)
            .map(i => nums(i + 1) - nums(i))
            .toList
        if !left then
            nums.last + predict(reduced, left)
        else
            nums.head - predict(reduced, left)

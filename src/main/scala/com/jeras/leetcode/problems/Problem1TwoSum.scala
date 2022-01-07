package com.jeras.leetcode.problems

/**
 * 1. Two Sum
 * https://leetcode.com/problems/two-sum/
 *
 * Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
 *
 * You may assume that each input would have exactly one solution, and you may not use the same element twice.
 *
 * You can return the answer in any order.
 *
 * ------------------
 *
 * Example 1:
 * Input: nums = [2,7,11,15], target = 9
 * Output: [0,1]
 * Output: Because nums[0] + nums[1] == 9, we return [0, 1].
 *
 * Example 2:
 * Input: nums = [3,2,4], target = 6
 * Output: [1,2]
 *
 * Example 3:
 * Input: nums = [3,3], target = 6
 * Output: [0,1]
 */

object Problem1TwoSum {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val numsIndexMap = scala.collection.mutable.Map.empty[Int, Int]

    for(i <- 0 until nums.size) {
      val numI = nums(i)
      numsIndexMap.get(target - numI) match {
        case Some(j) if i != j => return Array(i, j)
        case _ => numsIndexMap.put(numI, i)
      }
    }

    Array.empty
  }

  def twoSumV3(nums: Array[Int], target: Int): Array[Int] = {
    val numsIndex = nums.zipWithIndex
    val numsIndexMap = numsIndex.toMap

    numsIndex
      .collectFirst {
        case (numA, a) if numsIndexMap.get(target - numA).exists(_ != a) =>
          Array(a, numsIndexMap(target - numA))
      }
      .getOrElse(Array.empty)
  }

  def twoSumV2(nums: Array[Int], target: Int): Array[Int] = {
    for (i <- 0 until nums.size - 1) {
      val numJ = target - nums(i)
      val j = (i+1 until nums.size).find(nums(_) == numJ)
      if (j.isDefined) return Array(i, j.get)
    }

    Array.empty
  }

  def twoSumV1(nums: Array[Int], target: Int): Array[Int] = {
    for (i <- 0 until nums.size - 1) {
      for (j <- i+1 until nums.size) {
        if (nums(i) + nums(j) == target)
          return Array(i, j)
      }
    }

    Array.empty
  }

}

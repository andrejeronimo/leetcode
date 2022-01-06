/**
 * 997. Find the Town Judge
 * https://leetcode.com/problems/find-the-town-judge/
 *
 * In a town, there are n people labeled from 1 to n. There is a rumor that one of these people is secretly the town judge.
 *
 * If the town judge exists, then:
 *   1. The town judge trusts nobody.
 *   2. Everybody (except for the town judge) trusts the town judge.
 *   3. There is exactly one person that satisfies properties 1 and 2.
 *
 * You are given an array trust where trust[i] = [ai, bi] representing that the person labeled ai trusts the person labeled bi.
 *
 * Return the label of the town judge if the town judge exists and can be identified, or return -1 otherwise.
 *
 * ------------------
 *
 * Example 1:
 * Input: n = 2, trust = [[1,2]]
 * Output: 2
 *
 * Example 2:
 * Input: n = 3, trust = [[1,3],[2,3]]
 * Output: 3
 *
 * Example 3:
 * Input: n = 3, trust = [[1,3],[2,3],[3,1]]
 * Output: -1
 */

package com.jeras.leetcode.problems

import scala.collection.mutable.Map

object Problem997FindTheTownJudge {

  def findJudge(n: Int, trust: Array[Array[Int]]): Int = {
    trust
      .foldLeft((1 to n).map(_ -> 0).toMap) {
        case (trustCountMap, Array(personA, personB)) =>
          trustCountMap
            .updated(personB, trustCountMap(personB) + 1) // Person B is trusted by one more person
            .updated(personA, trustCountMap(personA) - 1) // Person A trusts someone, thus is not the judge
      }
      .find { case (_, trustCount) => trustCount == n - 1 }
      .map(_._1)
      .getOrElse(-1)
  }

  // Version 2
  def findJudgeV2(n: Int, trust: Array[Array[Int]]): Int = {
    if (n == 1) {
      1
    } else {

      val trustCountMap = Map[Int, Int]().withDefaultValue(0)

      trust.foreach {
        case Array(_, b) => trustCountMap(b) += 1
      }

      // Everybody (except for the town judge) trusts the town judge.
      val judgeCandidate = trustCountMap.find { case (_, count) => count == n - 1 }

      // The town judge trusts nobody.
      judgeCandidate match {
        case Some((judge, _)) if !trust.exists { case Array(a, _) => a == judge } => judge
        case _ => -1
      }
    }
  }

  // Version 1
  def findJudgeV1(n: Int, trust: Array[Array[Int]]): Int = {
    val trustMap = (1 to n).map(_ -> scala.collection.mutable.Set.empty[Int]).toMap

    trust.foreach {
      case Array(a, b) =>
        trustMap(a) += b
    }

    val judgeCandidate = trustMap.collect{ case (x, friends) if friends.isEmpty => x }.headOption

    judgeCandidate match {
      case Some(judge) =>
        val everyoneTrustsJudge = trustMap.forall{case (x, friends) => friends.contains(judge) || x == judge}
        if (everyoneTrustsJudge) judge else -1
      case _ => -1
    }

  }

}

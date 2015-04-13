/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import scala.collection.mutable.ArrayBuffer


object ZeroWidth {
  def replaceNode(it: Node, by: Node): Unit = {
    for (consumer <- it.consumers) {
      for (i <- 0 until consumer.inputs.size) {
        if (consumer.inputs(i) == it) {
          consumer.inputs(i) = by
          by.consumers += consumer
        }
      }
    }
  }

  def replaceNode(it: Node, by: Int): Unit = {
    replaceNode(it, it.inputs(by))
  }

  def none(node: Node): Unit = {}

  def binaryPartition(node: Node): (Node, Node) = {
    if (node.inputs(0).getWidth == 0) {
      return (node.inputs(0), node.inputs(1))
    }
    if (node.inputs(1).getWidth == 0) {
      return (node.inputs(1), node.inputs(0))
    }
    return null
  }

  def binaryTakeOther(node: Node): Unit = {
    val w0 = node.inputs(0).getWidth
    val w1 = node.inputs(1).getWidth
    if (w0 == 0) {
      replaceNode(node, 1)
    } else if (w1 == 0) {
      replaceNode(node, 0)
    }
  }



  def binaryInductZeroWithOtherWidth(zeroFactory: (BigInt, BitCount) => Node)(node: Node): Unit = {
    val partition = binaryPartition(node)
    if (partition != null) {
      Component.push(node.component)
      replaceNode(node, zeroFactory(0, partition._2.getWidth bit))
      Component.pop(node.component)
    }
  }

  def resizeImpl(zeroFactory: (BigInt, BitCount) => Node)(node: Node): Unit = {
    val w0 = node.inputs(0).getWidth
    if (w0 == 0) {
      Component.push(node.component)
      replaceNode(node, zeroFactory(0, node.getWidth bit))
      Component.pop(node.component)
    }
  }

  def shiftRightImpl(node: Node): Unit = {
    val w1 = node.inputs(1).getWidth
    if (w1 == 0) {
      Component.push(node.component)
      replaceNode(node, 0)
      Component.pop(node.component)
    }
  }
  s
  def shiftLeftImpl(zeroFactory: (BigInt, BitCount) => Node)(node: Node): Unit = {
    val w0 = node.inputs(0).getWidth
    val w1 = node.inputs(1).getWidth
    if (w0 == 0) {
      Component.push(node.component)
      replaceNode(node, zeroFactory(0, node.getWidth bit))
      Component.pop(node.component)
    } else if (w1 == 0) {
      Component.push(node.component)
      replaceNode(node, 0)
      Component.pop(node.component)
    }
  }

  def multiplexerImpl(node: Node): Unit = {
    val w0 = node.inputs(0).getWidth
    val w1 = node.inputs(1).getWidth
    if (w0 == 0) {
      replaceNode(node, 1)
    } else if (w1 == 0) {
      replaceNode(node, 0)
    }
  }


  def binaryThatIfBoth(thatFactory: => Node)(node: Node): Unit = {
    if (node.inputs(0).getWidth == 0 && node.inputs(1).getWidth == 0) {
      Component.push(node.component)
      replaceNode(node, thatFactory)
      Component.pop(node.component)
    }
  }


  def unaryShortCut(node: Node): Unit = {
    if (node.inputs(0).getWidth == 0) {
      replaceNode(node, 0)
    }
  }
  def unaryZero(node: Node): Unit = {
    if (node.inputs(0).getWidth == 0) {
      Component.push(node.component)
      replaceNode(node, u(0, 0 bit))
      Component.pop(node.component)
    }
  }
}

object InputNormalize {
  def none(node: Node): Unit = {

  }

  def regImpl(node: Node): Unit = {
    val targetWidth = node.getWidth
    Misc.normalizeResize(node, RegS.getDataInputId, targetWidth)
    if (node.asInstanceOf[Reg].isUsingReset) Misc.normalizeResize(node, RegS.getInitialValueId, targetWidth)
  }

  def memReadImpl(node: Node): Unit = {
    //not here
    //Misc.normalizeResize(node, MemReadSync.getAddressId, node.asInstanceOf[].addressWidth)
  }

  def memWriteImpl(node: Node): Unit = {
    Misc.normalizeResize(node, MemWrite.getDataId, node.getWidth)
    Misc.normalizeResize(node, MemWrite.getAddressId, node.asInstanceOf[Mem[_]].addressWidth)
  }

  def nodeWidth(node: Node): Unit = {
    val targetWidth = node.getWidth
    for (i <- 0 until node.inputs.size)
      Misc.normalizeResize(node, i, targetWidth)
  }

  def inputWidthMax(node: Node): Unit = {
    val targetWidth = Math.max(node.inputs(0).getWidth, node.inputs(1).getWidth)
    for (i <- 0 until node.inputs.size)
      Misc.normalizeResize(node, i, targetWidth)
  }
}

object WidthInfer {
  def inputMaxWidth(node: Node): Int = {
    node.inputs.foldLeft(-1)((best, n) => Math.max(best, if (n != null) n.getWidth else -1))
  }

  def multiplexImpl(node: Node): Int = {
    Math.max(node.inputs(1).getWidth, node.inputs(2).getWidth)
  }

  def regImpl(node: Node): Int = {
    val dataIn = node.inputs(RegS.getDataInputId)
    val init = node.inputs(RegS.getInitialValueId)
    math.max(if (dataIn != node) dataIn.getWidth else -1, if (node.asInstanceOf[Reg].isUsingReset) init.getWidth else -1)
  }

  def cumulateInputWidth(node: Node): Int = {
    node.inputs.foldLeft(0)((old, n) => old + Math.max(0, n.getWidth))
  }

  def intLit1Width(node: Node): Int = {
    node.inputs(1).asInstanceOf[IntLiteral].value.toInt
  }

  def input0Width(node: Node): Int = {
    node.inputs(0).getWidth
  }

  def shiftLeftWidth(node: Node): Int = node.inputs(0).getWidth + node.inputs(1).asInstanceOf[MinMaxProvider].maxValue.toInt
  def shiftRightWidth(node: Node): Int = Math.max(0, node.inputs(0).getWidth - node.inputs(1).asInstanceOf[MinMaxProvider].minValue.toInt)


  def oneWidth(node: Node): Int = 1

}


abstract class Node extends ContextUser with ScalaLocated with SpinalTagReady with GlobalDataUser {
  val consumers = new ArrayBuffer[Node]
  val inputs = new ArrayBuffer[Node]


  var widthWhenNotInferred = -1

  def getWidth: Int = {
    if (globalData.nodeAreInferringWidth) {
      inferredWidth
    } else {
      val isFirst = globalData.nodeGetWidthWalkedSet.isEmpty
      if (globalData.nodeGetWidthWalkedSet.contains(this))
        SpinalError(s"Can't calculate width of $this when design is in construction phase")

      globalData.nodeGetWidthWalkedSet += this
      var temp: Int = 0;
      if (isFirst) {
        try {
          temp = calcWidth
        } catch {
          case e: Exception => {
            globalData.nodeGetWidthWalkedSet.clear()
            throw e
          }
        }
      } else {
        temp = calcWidth
      }

      if (temp == -1) {
        globalData.nodeGetWidthWalkedSet.clear()
        SpinalError(s"Can't infer width because of unspecified width on ${this.getScalaLocationString}")
      }

      globalData.nodeGetWidthWalkedSet -= this

      if (isFirst) globalData.nodeGetWidthWalkedSet.clear()
      if (widthWhenNotInferred != -1 && widthWhenNotInferred != temp) SpinalError(s"getWidth result differ from last call $getScalaLocationString")
      widthWhenNotInferred = temp
      temp
    }
  }

  def calcWidth: Int


  def inferWidth: Boolean = {
    val newWidth: Int = calcWidth
    if (newWidth == -1) {
      return true
    } else if (newWidth != inferredWidth) {
      inferredWidth = newWidth
      return true;
    } else {
      return false
    }
  }


  var inferredWidth = -1

  def checkInferedWidth: String = null

  def normalizeInputs: Unit = {

  }

  def simplifyNode: Unit = {}

  def setInput(node: Node): Unit = {
    inputs(0) = node
  }


  def getClassIdentifier: String = this.getClass.getSimpleName

  def isInBlackBoxTree = component.isInBlackBoxTree

  def nonRecursiveToString(): String = {
    toString()
  }
}

object NoneNode {
  def apply() = new NoneNode
}

class NoneNode extends Node {
  override def calcWidth: Int = 0
}
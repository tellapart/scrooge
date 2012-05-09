package com.twitter.scrooge

import java.util.Arrays
import scala.collection.mutable
import com.twitter.finagle.Service
import com.twitter.util.Future
import org.apache.thrift.TApplicationException
import org.apache.thrift.protocol._
import org.apache.thrift.transport.{TMemoryInputTransport, TMemoryBuffer}

/**
 * Common code used by any finagle thrift service code generated by scrooge.
 */
trait FinagleThriftService extends Service[Array[Byte], Array[Byte]] {
  def protocolFactory: TProtocolFactory

  protected val functionMap = new mutable.HashMap[String, (TProtocol, Int) => Future[Array[Byte]]]()

  def exception(name: String, seqid: Int, code: Int, message: String): Future[Array[Byte]] = {
    Future {
      val x = new TApplicationException(code, message)
      val memoryBuffer = new TMemoryBuffer(512)
      val oprot = protocolFactory.getProtocol(memoryBuffer)

      oprot.writeMessageBegin(new TMessage(name, TMessageType.EXCEPTION, seqid))
      x.write(oprot)
      oprot.writeMessageEnd()
      oprot.getTransport.flush()
      Arrays.copyOfRange(memoryBuffer.getArray, 0, memoryBuffer.length)
    }
  }

  def reply(name: String, seqid: Int, result: ThriftStruct): Future[Array[Byte]] = {
    Future {
      val memoryBuffer = new TMemoryBuffer(512)
      val oprot = protocolFactory.getProtocol(memoryBuffer)

      oprot.writeMessageBegin(new TMessage(name, TMessageType.REPLY, seqid))
      result.write(oprot)
      oprot.writeMessageEnd()

      Arrays.copyOfRange(memoryBuffer.getArray, 0, memoryBuffer.length)
    }
  }

  def apply(request: Array[Byte]): Future[Array[Byte]] = {
    val inputTransport = new TMemoryInputTransport(request)
    val iprot = protocolFactory.getProtocol(inputTransport)

    try {
      val msg = iprot.readMessageBegin()
      functionMap.get(msg.name) match {
        case Some(f) =>
          f(iprot, msg.seqid)
        case None =>
          TProtocolUtil.skip(iprot, TType.STRUCT)
          iprot.readMessageEnd()
          exception(msg.name, msg.seqid, TApplicationException.UNKNOWN_METHOD, "Invalid method name: '" + msg.name + "'")
      }
    } catch {
      case e: Exception => Future.exception(e)
    }
  }
}
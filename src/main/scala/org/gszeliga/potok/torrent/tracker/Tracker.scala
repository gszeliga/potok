package org.gszeliga.potok.torrent.tracker

import io.netty.bootstrap.Bootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioDatagramChannel
import io.netty.channel.socket.nio.NioDatagramChannelConfig
import io.netty.channel.socket.nio.NioDatagramChannelConfig
import io.netty.channel.ChannelOption
import io.netty.channel.socket.DatagramPacket
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.ChannelHandlerContext
import java.net.InetSocketAddress
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled
import io.netty.channel.Channel
import io.netty.channel.ChannelFutureListener
import io.netty.channel.ChannelFuture
import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.DatagramChannel
import io.netty.handler.logging.LoggingHandler
import io.netty.handler.logging.LogLevel
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

sealed case class Message(content: ByteBuf)

//Incoming message handler starting by Datagram
class TorrentTrackerHandler(f: (InetSocketAddress, Message) => Any) extends SimpleChannelInboundHandler[DatagramPacket] {
  def channelRead0(ctx: ChannelHandlerContext, msg: DatagramPacket) = {
    println("Handler....")
    f(msg.sender, Message(msg.content))
  }
}

class TrackerRef(private val ch: ChannelFuture) {
  def announce(target: InetSocketAddress, msg: Array[Byte]) = {
    println("Announcing...")
    ch.channel.writeAndFlush(new DatagramPacket(Unpooled.copiedBuffer(msg), target))
    println("Announcing...done")
  }
}

class TrackerToken(private val ch: ChannelFuture) {
  def stop = {
    println("Stoping...")
    ch.channel().closeFuture().cancel(true)
    Thread.sleep(1000)
    println("Stoping...DONE")
  }
}

class Tracker(port: Int, f: (InetSocketAddress, Message) => Any) {

  def run(success: TrackerRef => Unit) = {
    val group = new NioEventLoopGroup
    val bootstrap = new Bootstrap

    bootstrap.group(group).option[java.lang.Boolean](ChannelOption.SO_BROADCAST, false).channel(classOf[NioDatagramChannel]).handler(new ChannelInitializer[DatagramChannel] {
      def initChannel(chn: DatagramChannel) = {
        chn.pipeline().addLast("logger", new LoggingHandler(LogLevel.DEBUG))
        chn.pipeline().addLast("torrent", new TorrentTrackerHandler(f))
      }
    })

    val cf = bootstrap.bind(port);

    cf.addListener(new ChannelFutureListener {
      def operationComplete(future: ChannelFuture) {

        if (future.isSuccess()) {
          println("Arrancado....")
          success(new TrackerRef(future))
        }
      }
    })
    
    Future {
      try {
        println("sync....")
        cf.channel().closeFuture().sync()
        println("sync....OUT!!")
      } finally {
        println("Shutting down....")
        group.shutdownGracefully();
      }

    }

    new TrackerToken(cf)
  }
}

object Tracker {
  def apply(port: Int)(f: (InetSocketAddress, Message) => Unit)(success: TrackerRef => Unit) = new Tracker(port, f).run(success)
}
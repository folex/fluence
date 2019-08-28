/*
 * Copyright 2018 Fluence Labs Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package proto3.tendermint

@SerialVersionUID(0L)
final case class BlockMeta(
  blockID: _root_.scala.Option[proto3.tendermint.BlockID] = None,
  header: _root_.scala.Option[proto3.tendermint.Header] = None
) extends scalapb.GeneratedMessage with scalapb.Message[BlockMeta] with scalapb.lenses.Updatable[BlockMeta] {

  @transient
  private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
  private[this] def __computeSerializedValue(): _root_.scala.Int = {
    var __size = 0
    if (blockID.isDefined) {
      val __value = blockID.get
      __size += 1 + _root_.com.google.protobuf.CodedOutputStream
        .computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
    };
    if (header.isDefined) {
      val __value = header.get
      __size += 1 + _root_.com.google.protobuf.CodedOutputStream
        .computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
    };
    __size
  }
  final override def serializedSize: _root_.scala.Int = {
    var read = __serializedSizeCachedValue
    if (read == 0) {
      read = __computeSerializedValue()
      __serializedSizeCachedValue = read
    }
    read
  }

  def writeTo(`_output__`: _root_.com.google.protobuf.CodedOutputStream): _root_.scala.Unit = {
    blockID.foreach { __v =>
      val __m = __v
      _output__.writeTag(1, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
    header.foreach { __v =>
      val __m = __v
      _output__.writeTag(2, 2)
      _output__.writeUInt32NoTag(__m.serializedSize)
      __m.writeTo(_output__)
    };
  }

  def mergeFrom(`_input__`: _root_.com.google.protobuf.CodedInputStream): proto3.tendermint.BlockMeta = {
    var __blockID = this.blockID
    var __header = this.header
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 10 =>
          __blockID = Option(
            _root_.scalapb.LiteParser
              .readMessage(_input__, __blockID.getOrElse(proto3.tendermint.BlockID.defaultInstance))
          )
        case 18 =>
          __header = Option(
            _root_.scalapb.LiteParser
              .readMessage(_input__, __header.getOrElse(proto3.tendermint.Header.defaultInstance))
          )
        case tag => _input__.skipField(tag)
      }
    }
    proto3.tendermint.BlockMeta(
      blockID = __blockID,
      header = __header
    )
  }
  def getBlockID: proto3.tendermint.BlockID = blockID.getOrElse(proto3.tendermint.BlockID.defaultInstance)
  def clearBlockID: BlockMeta = copy(blockID = None)
  def withBlockID(__v: proto3.tendermint.BlockID): BlockMeta = copy(blockID = Option(__v))
  def getHeader: proto3.tendermint.Header = header.getOrElse(proto3.tendermint.Header.defaultInstance)
  def clearHeader: BlockMeta = copy(header = None)
  def withHeader(__v: proto3.tendermint.Header): BlockMeta = copy(header = Option(__v))

  def getFieldByNumber(__fieldNumber: _root_.scala.Int): _root_.scala.Any = {
    (__fieldNumber: @ _root_.scala.unchecked) match {
      case 1 => blockID.orNull
      case 2 => header.orNull
    }
  }

  def getField(__field: _root_.scalapb.descriptors.FieldDescriptor): _root_.scalapb.descriptors.PValue = {
    _root_.scala.Predef.require(__field.containingMessage eq companion.scalaDescriptor)
    (__field.number: @ _root_.scala.unchecked) match {
      case 1 => blockID.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
      case 2 => header.map(_.toPMessage).getOrElse(_root_.scalapb.descriptors.PEmpty)
    }
  }
  def toProtoString: _root_.scala.Predef.String = _root_.scalapb.TextFormat.printToUnicodeString(this)
  def companion = proto3.tendermint.BlockMeta
}

object BlockMeta extends scalapb.GeneratedMessageCompanion[proto3.tendermint.BlockMeta] {
  implicit def messageCompanion: scalapb.GeneratedMessageCompanion[proto3.tendermint.BlockMeta] = this

  def fromFieldsMap(
    __fieldsMap: scala.collection.immutable.Map[_root_.com.google.protobuf.Descriptors.FieldDescriptor,
                                                _root_.scala.Any]
  ): proto3.tendermint.BlockMeta = {
    _root_.scala.Predef.require(__fieldsMap.keys.forall(_.getContainingType() == javaDescriptor),
                                "FieldDescriptor does not match message type.")
    val __fields = javaDescriptor.getFields
    proto3.tendermint.BlockMeta(
      __fieldsMap.get(__fields.get(0)).asInstanceOf[_root_.scala.Option[proto3.tendermint.BlockID]],
      __fieldsMap.get(__fields.get(1)).asInstanceOf[_root_.scala.Option[proto3.tendermint.Header]]
    )
  }
  implicit def messageReads: _root_.scalapb.descriptors.Reads[proto3.tendermint.BlockMeta] =
    _root_.scalapb.descriptors.Reads {
      case _root_.scalapb.descriptors.PMessage(__fieldsMap) =>
        _root_.scala.Predef.require(__fieldsMap.keys.forall(_.containingMessage == scalaDescriptor),
                                    "FieldDescriptor does not match message type.")
        proto3.tendermint.BlockMeta(
          __fieldsMap
            .get(scalaDescriptor.findFieldByNumber(1).get)
            .flatMap(_.as[_root_.scala.Option[proto3.tendermint.BlockID]]),
          __fieldsMap
            .get(scalaDescriptor.findFieldByNumber(2).get)
            .flatMap(_.as[_root_.scala.Option[proto3.tendermint.Header]])
        )
      case _ => throw new RuntimeException("Expected PMessage")
    }

  def javaDescriptor: _root_.com.google.protobuf.Descriptors.Descriptor =
    TendermintProto.javaDescriptor.getMessageTypes.get(11)
  def scalaDescriptor: _root_.scalapb.descriptors.Descriptor = TendermintProto.scalaDescriptor.messages(11)

  def messageCompanionForFieldNumber(__number: _root_.scala.Int): _root_.scalapb.GeneratedMessageCompanion[_] = {
    var __out: _root_.scalapb.GeneratedMessageCompanion[_] = null
    (__number: @ _root_.scala.unchecked) match {
      case 1 => __out = proto3.tendermint.BlockID
      case 2 => __out = proto3.tendermint.Header
    }
    __out
  }
  lazy val nestedMessagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_]] = Seq.empty

  def enumCompanionForFieldNumber(__fieldNumber: _root_.scala.Int): _root_.scalapb.GeneratedEnumCompanion[_] =
    throw new MatchError(__fieldNumber)
  lazy val defaultInstance = proto3.tendermint.BlockMeta(
    )
  implicit class BlockMetaLens[UpperPB](_l: _root_.scalapb.lenses.Lens[UpperPB, proto3.tendermint.BlockMeta])
      extends _root_.scalapb.lenses.ObjectLens[UpperPB, proto3.tendermint.BlockMeta](_l) {

    def blockID: _root_.scalapb.lenses.Lens[UpperPB, proto3.tendermint.BlockID] =
      field(_.getBlockID)((c_, f_) => c_.copy(blockID = Option(f_)))

    def optionalBlockID: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Option[proto3.tendermint.BlockID]] =
      field(_.blockID)((c_, f_) => c_.copy(blockID = f_))

    def header: _root_.scalapb.lenses.Lens[UpperPB, proto3.tendermint.Header] =
      field(_.getHeader)((c_, f_) => c_.copy(header = Option(f_)))

    def optionalHeader: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Option[proto3.tendermint.Header]] =
      field(_.header)((c_, f_) => c_.copy(header = f_))
  }
  final val BLOCKID_FIELD_NUMBER = 1
  final val HEADER_FIELD_NUMBER = 2
}
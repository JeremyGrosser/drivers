--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;

package ENC28J60
   with Preelaborate
is
   type Any_SPI_Transfer is not null access procedure (Data : in out UInt8);
   type Any_Chip_Select is not null access procedure (High : Boolean);

   type Device is record
      SPI_Transfer : Any_SPI_Transfer;
      CS : Any_Chip_Select;

      Bank : UInt8 := 0;
      Next_Packet_Ptr : UInt16 := 0;
      Init_Done : Boolean := False;
   end record;

   subtype MAC_Address is UInt8_Array (1 .. 6);

   procedure Initialize
      (This : in out Device;
       MAC  : MAC_Address)
   with Post => This.Init_Done;

   subtype Packet_Data is UInt8_Array (1 .. 1500);

   procedure Packet_Transmit
      (This : in out Device;
       Data : Packet_Data;
       Last : Natural)
   with Pre => This.Init_Done and then Last <= Data'Last;

   procedure Packet_Receive
      (This : in out Device;
       Data : out Packet_Data;
       Last : out Natural)
   with Pre  => This.Init_Done,
        Post => Last <= Data'Last;

   procedure Get_Link_Status
      (This : in out Device;
       Up   : out Boolean)
   with Pre => This.Init_Done;

   procedure Get_Silicon_Revision
      (This : in out Device;
       Rev  : out UInt8)
   with Pre => This.Init_Done;

private

   procedure SPI_Write
      (This : Device;
       Data : UInt8);

   procedure Read_Op
      (This : Device;
       Op   : UInt8;
       Addr : UInt8;
       Data : out UInt8);

   procedure Write_Op
      (This : Device;
       Op   : UInt8;
       Addr : UInt8;
       Data : UInt8);

   procedure Set_Bank
      (This : in out Device;
       Addr : UInt8);

   procedure Write
      (This : in out Device;
       Addr : UInt8;
       Data : UInt8);

   procedure Write_16
      (This : in out Device;
       Addr : UInt8;
       Data : UInt16);

   procedure Read
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt8);

   procedure Read_16
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt16);

   procedure Phy_Write
      (This : in out Device;
       Addr : UInt8;
       Data : UInt16);

   procedure Phy_Read
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt16);

   procedure Write_Buffer
      (This : Device;
       Data : UInt8_Array);

   procedure Read_Buffer
      (This : Device;
       Data : out UInt8_Array);

end ENC28J60;

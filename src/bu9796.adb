--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body BU9796 is

   procedure Initialize
      (This    : in out Device;
       Status  : out I2C_Status)
   is
   begin
      This.Write_Command (2#0011_1100#, Status); --  lowest power consumption
      if Status /= Ok then
         return;
      end if;

      This.Write_Command (2#0100_1000#, Status); --  display on, 1/3 bias
      if Status /= Ok then
         return;
      end if;
   end Initialize;

   procedure Write_Command
      (This    : in out Device;
       Cmd     : UInt8;
       Status  : out I2C_Status)
   is
      Data : constant I2C_Data (1 .. 1) := (1 => Cmd);
   begin
      This.Port.Master_Transmit (This.Addr, Data, Status, This.Timeout);
   end Write_Command;

   procedure Update
      (This    : in out Device;
       Status  : out I2C_Status)
   is
   begin
      This.Port.Master_Transmit (This.Addr, This.Buffer, Status);
   end Update;

end BU9796;

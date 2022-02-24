--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Unchecked_Conversion;

package body PMS is

   function Name
      (F : Field)
      return String
   is
   begin
      case F is
         when Start        => return "(start)";
         when Length       => return "(length)";
         when CF1_PM_1     => return "PM1.0";
         when CF1_PM_2_5   => return "PM2.5";
         when CF1_PM_10    => return "PM10";
         when ATM_PM_1     => return "PM1.0 atmosphere";
         when ATM_PM_2_5   => return "PM2.5 atmosphere";
         when ATM_PM_10    => return "PM10 atmosphere";
         when PART_0_3     => return "0.3";
         when PART_0_5     => return "0.5";
         when PART_1       => return "1.0";
         when PART_2_5     => return "2.5";
         when PART_5       => return "5.0";
         when PART_10      => return "10.0";
         when Reserved     => return "(reserved)";
         when Checksum     => return "(checksum)";
      end case;
   end Name;

   function Calculate_Checksum
      (Data : Frame)
      return UInt16
   is
      Sum : UInt16 := 0;
   begin
      for I in Start .. Reserved loop
         Sum := Sum + Data (I);
      end loop;
      return Sum;
   end Calculate_Checksum;

   procedure Receive
      (Port    : not null Any_UART_Port;
       Data    : out Frame;
       Status  : out UART_Status;
       Timeout : Natural := 1_000)
   is
      function Byte_Swap (Word : UInt16) return UInt16
      is (Shift_Right (Word, 8) or Shift_Left (Word, 8));

      subtype Raw_Frame is UART_Data_8b (1 .. Frame'Length * 2);
      function To_Frame is new Ada.Unchecked_Conversion
         (Raw_Frame, Frame);

      S  : UART_Status;
      RF : Raw_Frame;
      F  : Frame;
   begin
      Port.Receive (RF, S, Timeout);
      Status := S;
      if S /= Ok then
         return;
      end if;

      F := To_Frame (RF);
      for I in F'Range loop
         F (I) := Byte_Swap (F (I));
      end loop;
      Data := F;

      if F (Checksum) /= Calculate_Checksum (F) then
         Status := Err_Error;
      end if;
   end Receive;

end PMS;

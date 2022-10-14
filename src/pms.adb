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

   function Valid_Checksum
      (Data : Raw_Frame)
      return Boolean
   is
      Check : UInt16;
      Sum   : UInt16 := 0;
   begin
      Check := Shift_Left (UInt16 (Data (Data'Last - 1)), 8);
      Check := Check or UInt16 (Data (Data'Last));

      for I in Data'First .. Data'Last - 2 loop
         Sum := Sum + UInt16 (Data (I));
      end loop;
      return Sum = Check;
   end Valid_Checksum;

   function Convert is new Ada.Unchecked_Conversion (Raw_Frame, Frame);
   function Byte_Swap (Word : UInt16) return UInt16
   is (Shift_Right (Word, 8) or Shift_Left (Word, 8));

   procedure Receive
      (Port    : not null Any_UART_Port;
       Data    : out Frame;
       Status  : out UART_Status;
       Timeout : Natural := 1_000)
   is
      RF    : Raw_Frame;
      Valid : Boolean;
   begin
      Port.Receive (RF, Status, Timeout);
      if Status /= Ok then
         return;
      end if;

      Data := To_Frame (RF, Valid);
      if not Valid then
         Status := Err_Error;
      end if;
   end Receive;

   function To_Frame
      (Data  : Raw_Frame;
       Valid : out Boolean)
       return Frame
   is
      F : Frame;
   begin
      Valid := Valid_Checksum (Data);
      F := Convert (Data);
      for I in F'Range loop
         F (I) := Byte_Swap (F (I));
      end loop;
      return F;
   end To_Frame;

end PMS;

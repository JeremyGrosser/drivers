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
      (Raw_Data : UInt8_Array)
      return Boolean
   is
      Check : UInt16;
      Sum   : UInt16 := 0;
   begin
      Check := Shift_Left (UInt16 (Raw_Data (Raw_Data'Last - 1)), 8);
      Check := Check or UInt16 (Raw_Data (Raw_Data'Last));

      for I in Raw_Data'First .. Raw_Data'Last - 2 loop
         Sum := Sum + UInt16 (Raw_Data (I));
      end loop;
      return Sum = Check;
   end Valid_Checksum;

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

      RF : Raw_Frame;
      F  : Frame;
   begin
      Port.Receive (RF, Status, Timeout);
      if Status /= Ok then
         return;
      end if;

      if not Valid_Checksum (UInt8_Array (RF)) then
         Status := Err_Error;
         return;
      end if;

      F := To_Frame (RF);
      for I in F'Range loop
         F (I) := Byte_Swap (F (I));
      end loop;
      Data := F;
   end Receive;

end PMS;

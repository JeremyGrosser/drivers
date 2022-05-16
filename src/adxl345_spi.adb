--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Assertions;

package body ADXL345_SPI is

   procedure SPI_Read
      (Reg : HAL.UInt8;
       Val : out HAL.UInt8_Array)
   is
      use Ada.Assertions;
      use HAL.SPI;
      Data : SPI_Data_8b (1 .. 1);
      Status : SPI_Status;
   begin
      CS.Clear;
      if Val'Length > 1 then
         Data (1) := Reg or 2#1100_0000#;
      else
         Data (1) := Reg or 2#1000_0000#;
      end if;

      Port.Transmit (Data, Status, Timeout => 0);
      Assert (Status = Ok);

      Port.Receive (Data, Status, Timeout => 0);
      Assert (Status = Ok);

      for I in Val'Range loop
         Data (1) := 0;
         Port.Transmit (Data, Status, Timeout => 0);
         Assert (Status = Ok);

         Port.Receive (Data, Status, Timeout => 0);
         Val (I) := Data (1);
         Assert (Status = Ok);
      end loop;

      CS.Set;
   end SPI_Read;

   procedure SPI_Write
      (Reg : UInt8;
       Val : UInt8_Array)
   is
      use Ada.Assertions;
      use HAL.SPI;
      Data : SPI_Data_8b (1 .. 1);
      Status : SPI_Status;
   begin
      CS.Clear;
      if Val'Length > 1 then
         Data (1) := Reg or 2#0100_0000#;
      else
         Data (1) := Reg;
      end if;

      Port.Transmit (Data, Status, Timeout => 0);
      Assert (Status = Ok);
      Port.Receive (Data, Status, Timeout => 0);
      Assert (Status = Ok);

      for I in Val'Range loop
         Data (1) := Val (I);
         Port.Transmit (Data, Status, Timeout => 0);
         Assert (Status = Ok);

         Port.Receive (Data, Status, Timeout => 0);
         Assert (Status = Ok);
      end loop;

      CS.Set;
   end SPI_Write;

end ADXL345_SPI;

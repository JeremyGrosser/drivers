--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body HT16K33 is
   procedure Initialize
      (This : in out Device)
   is
      Oscillator_On : constant I2C_Data (1 .. 1) := (1 => 16#21#);
      Display_On    : constant I2C_Data (1 .. 1) := (1 => 16#81#);
      Row_Output    : constant I2C_Data (1 .. 1) := (1 => 16#A0#);
      Status        : I2C_Status := Err_Error;
   begin
      This.Port.Master_Transmit (This.Address, Oscillator_On, Status, Timeout => 10);
      This.Port.Master_Transmit (This.Address, Display_On, Status, Timeout => 10);
      This.Port.Master_Transmit (This.Address, Row_Output, Status, Timeout => 10);
      Set_Brightness (This, Brightness_Level'Last);
      Update (This);
   end Initialize;

   procedure Set_Brightness
      (This  : in out Device;
       Level : Brightness_Level)
   is
      Data   : constant I2C_Data (1 .. 1) := (1 => 16#E0# or Level);
      Status : I2C_Status := Err_Error;
   begin
      This.Port.Master_Transmit (This.Address, Data, Status, Timeout => 10);
   end Set_Brightness;

   procedure Update
      (This : in out Device)
   is
      Status : I2C_Status := Err_Error;
   begin
      This.Port.Master_Transmit (This.Address, This.Buffer, Status, Timeout => 100);
   end Update;

   procedure Set
      (This : in out Device;
       Num  : Output_Index)
   is
      Index : constant Positive := (Num / 8) + 1;
   begin
      This.Buffer (Index) := This.Buffer (Index) or Shift_Left (1, Num mod 8);
   end Set;

   procedure Clear
      (This : in out Device;
       Num  : Output_Index)
   is
      Index : constant Positive := (Num / 8) + 1;
   begin
      This.Buffer (Index) := This.Buffer (Index) and not Shift_Left (1, Num mod 8);
   end Clear;

   procedure Clear_All
      (This : in out Device)
   is
   begin
      This.Buffer := (others => 0);
   end Clear_All;

end HT16K33;

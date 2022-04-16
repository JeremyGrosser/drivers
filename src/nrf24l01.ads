--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  nRF24L01+ 2.4 GHz GFSK modem
--
--  This driver disables the Enhanced ShockBurst™ acknowledgement and checksum
--  features and just acts like a dumb GFSK modem. You will need to add your
--  own checksum, error correction codes, and acknowledgement protocol as
--  needed.
--
--  Think of the NRF_Address as more of a preamble than a way of ensuring that
--  frames get to the right place. Add a header to your data if this is
--  important to your application.
--
--  Frequency hopping, data whitening and parity are good ideas.
--
with Ada.Unchecked_Conversion;
with HAL.SPI; use HAL.SPI;
with HAL; use HAL;
with HAL.GPIO;

package NRF24L01 is

   type Device
      (Port : HAL.SPI.Any_SPI_Port;
       CS   : HAL.GPIO.Any_GPIO_Point;
       CE   : HAL.GPIO.Any_GPIO_Point)
   is tagged private;

   subtype NRF_Address_Width is Positive range 3 .. 5;

   type NRF_Address
      (Width : NRF_Address_Width)
   is record
      Addr : UInt8_Array (1 .. Width);
   end record;

   type NRF_Channel is range 2_400 .. 2_527;
   --  MHz, default 2_476

   type NRF_Payload_Length is range 1 .. 32;

   type NRF_Transmit_Power is
      (Low_Power,    --  -18 dBm
       Medium_Power, --  -12 dBm
       High_Power,   --   -6 dBm
       Max_Power);   --    0 dBm (default)

   type NRF_Data_Rate is
      (Low_Rate,     --  125 Kbps
       Medium_Rate,  --    1 Mbps
       High_Rate);   --    2 Mbps (default)

   procedure Initialize
      (This : in out Device);

   procedure Interrupt
      (This : in out Device);
   --  Interrupt must be called upon the falling edge of the IRQ pin. Failure
   --  to do so will make Receive stop working and Transmit may keep the
   --  amplifier turned on for too long and and damage your chip.

   procedure Set_Channel
      (This : in out Device;
       MHz  : NRF_Channel);

   procedure Set_Data_Rate
      (This : in out Device;
       Rate : NRF_Data_Rate);

   function Is_Transmitting
      (This : Device)
      return Boolean;

   procedure Transmit
      (This   : in out Device;
       Addr   : NRF_Address;
       Data   : UInt8_Array;
       Power  : NRF_Transmit_Power := Max_Power)
   with Pre => not This.Is_Transmitting;

   procedure Listen
      (This   : in out Device;
       Addr   : NRF_Address;
       Length : NRF_Payload_Length);

   procedure Receive
      (This : in out Device;
       Data : out UInt8_Array);

   function Data_Ready
      (This : in out Device)
      return Natural;
   --  Number of frames waiting in the receive FIFO.
   --  Don't call Receive if Data_Ready = 0

private

   type NRF_Mode is (Idle, Transmitting, Receiving);

   type Device
      (Port : HAL.SPI.Any_SPI_Port;
       CS   : HAL.GPIO.Any_GPIO_Point;
       CE   : HAL.GPIO.Any_GPIO_Point)
   is tagged record
      Mode  : NRF_Mode := Idle;
      RX_DR : Natural with Atomic;
      Rate_Low  : Boolean;
      Rate_High : Boolean;
   end record;

   type Register is
      (CONFIG,
       EN_AA,
       EN_RXADDR,
       SETUP_AW,
       SETUP_RETR,
       RF_CH,
       RF_SETUP,
       STATUS,
       OBSERVE_TX,
       RPD,
       RX_ADDR_P0,
       RX_ADDR_P1,
       RX_ADDR_P2,
       RX_ADDR_P3,
       RX_ADDR_P4,
       RX_ADDR_P5,
       TX_ADDR,
       RX_PW_P0,
       RX_PW_P1,
       RX_PW_P2,
       RX_PW_P3,
       RX_PW_P4,
       RX_PW_P5,
       FIFO_STATUS,
       DYNPD,
       FEATURE);
   for Register use
      (CONFIG        => 16#00#,
       EN_AA         => 16#01#,
       EN_RXADDR     => 16#02#,
       SETUP_AW      => 16#03#,
       SETUP_RETR    => 16#04#,
       RF_CH         => 16#05#,
       RF_SETUP      => 16#06#,
       STATUS        => 16#07#,
       OBSERVE_TX    => 16#08#,
       RPD           => 16#09#,
       RX_ADDR_P0    => 16#0A#,
       RX_ADDR_P1    => 16#0B#,
       RX_ADDR_P2    => 16#0C#,
       RX_ADDR_P3    => 16#0D#,
       RX_ADDR_P4    => 16#0E#,
       RX_ADDR_P5    => 16#0F#,
       TX_ADDR       => 16#10#,
       RX_PW_P0      => 16#11#,
       RX_PW_P1      => 16#12#,
       RX_PW_P2      => 16#13#,
       RX_PW_P3      => 16#14#,
       RX_PW_P4      => 16#15#,
       RX_PW_P5      => 16#16#,
       FIFO_STATUS   => 16#17#,
       DYNPD         => 16#1C#,
       FEATURE       => 16#1D#);

   type STATUS_Register is record
      RX_DR    : Boolean := False;
      TX_DS    : Boolean := False;
      MAX_RT   : Boolean := False;
      RX_P_NO  : UInt3   := 0;
      TX_FULL  : Boolean := False;
   end record
      with Size => 8;
   for STATUS_Register use record
      RX_DR    at 0 range 6 .. 6;
      TX_DS    at 0 range 5 .. 5;
      MAX_RT   at 0 range 4 .. 4;
      RX_P_NO  at 0 range 1 .. 3;
      TX_FULL  at 0 range 0 .. 0;
   end record;

   function To_STATUS_Register is new Ada.Unchecked_Conversion
      (Source => UInt8,
       Target => STATUS_Register);

   type CONFIG_PRIM_RX_Field is (PTX, PRX)
      with Size => 1;

   type CONFIG_Register is record
      MASK_RX_DR  : Boolean := False;
      MASK_TX_DS  : Boolean := False;
      MASK_MAX_RT : Boolean := False;
      EN_CRC      : Boolean := True;
      CRCO        : Boolean := False;
      PWR_UP      : Boolean := False;
      PRIM_RX     : CONFIG_PRIM_RX_Field := PTX;
   end record
      with Size => 8;
   for CONFIG_Register use record
      MASK_RX_DR  at 0 range 6 .. 6;
      MASK_TX_DS  at 0 range 5 .. 5;
      MASK_MAX_RT at 0 range 4 .. 4;
      EN_CRC      at 0 range 3 .. 3;
      CRCO        at 0 range 2 .. 2;
      PWR_UP      at 0 range 1 .. 1;
      PRIM_RX     at 0 range 0 .. 0;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
      (Source => CONFIG_Register,
       Target => UInt8);

   type RF_SETUP_Register is record
      CONT_WAVE : Boolean := False;
      RF_DR_LOW : Boolean := False;
      PLL_LOCK : Boolean := False;
      RF_DR_HIGH : Boolean := True;
      RF_PWR : UInt2 := 2#11#;
   end record
      with Size => 8;
   for RF_SETUP_Register use record
      CONT_WAVE   at 0 range 7 .. 7;
      RF_DR_LOW   at 0 range 5 .. 5;
      PLL_LOCK    at 0 range 4 .. 4;
      RF_DR_HIGH  at 0 range 3 .. 3;
      RF_PWR      at 0 range 1 .. 2;
   end record;

   function To_UInt8 is new Ada.Unchecked_Conversion
      (Source => RF_SETUP_Register,
       Target => UInt8);

   procedure SPI_Transfer
      (This : in out Device;
       Data : in out SPI_Data_8b);

   procedure W_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : UInt8_Array);

   procedure W_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : UInt8);

   procedure R_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : out UInt8);

   procedure FLUSH_TX
      (This : in out Device);

   procedure FLUSH_RX
      (This : in out Device);

   procedure W_TX_PAYLOAD
      (This : in out Device;
       Data : UInt8_Array);

   procedure R_RX_PAYLOAD
      (This : in out Device;
       Data : out UInt8_Array);

   procedure NOP
      (This   : in out Device;
       Status : out STATUS_Register);

   procedure Clear_Status
      (This : in out Device);

   procedure Set_Transmit_Address
      (This : in out Device;
       Addr : NRF_Address);

   procedure Set_Receive_Address
      (This : in out Device;
       Addr : NRF_Address);

end NRF24L01;

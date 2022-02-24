with HAL; use HAL;
with NRF24L01_IO;

package NRF24L01_Registers is

   type PRIM_RX_Field is (PTX, PRX)
      with Size => 1;
   type CONFIG_Register is record
      MASK_RX_DR  : Boolean := False;
      MASK_TX_DS  : Boolean := False;
      MASK_MAX_RT : Boolean := False;
      EN_CRC      : Boolean := True;
      CRCO        : Boolean := False;
      PWR_UP      : Boolean := False;
      PRIM_RX     : PRIM_RX_Field := PTX;
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

   type Data_Pipe is (P0, P1, P2, P3, P4, P5, Reserved_6, Reserved_7);

   type EN_AA_Register is array (Data_Pipe) of Boolean
      with Component_Size => 1, Size => 8;

   type EN_RXADDR_Register is array (Data_Pipe) of Boolean
      with Component_Size => 1, Size => 8;

   type SETUP_AW_Register is (AW_3_Bytes, AW_4_Bytes, AW_5_Bytes)
      with Size => 8;
   for SETUP_AW_Register use
      (AW_3_Bytes => 2#01#,
       AW_4_Bytes => 2#10#,
       AW_5_Bytes => 2#11#);

   type SETUP_RETR_Register is record
      ARD : UInt4 := 0;
      ARC : UInt4 := 2#0011#;
   end record
      with Size => 8;
   for SETUP_RETR_Register use record
      ARD at 0 range 4 .. 7;
      ARC at 0 range 0 .. 3;
   end record;

   type RF_CH_Register is range 0 .. 127
      with Size => 8;

   type RF_SETUP_Register is record
      CONT_WAVE  : Boolean := False;
      RF_DR_LOW  : Boolean := False;
      PLL_LOCK   : Boolean := False;
      RF_DR_HIGH : Boolean := True;
      RF_PWR     : UInt2 := 3;
   end record
      with Size => 8;
   for RF_SETUP_Register use record
      CONT_WAVE   at 0 range 7 .. 7;
      RF_DR_LOW   at 0 range 5 .. 5;
      PLL_LOCK    at 0 range 4 .. 4;
      RF_DR_HIGH  at 0 range 3 .. 3;
      RF_PWR      at 0 range 1 .. 2;
   end record;

   type OBSERVE_TX_Register is record
      PLOS_CNT : UInt4 := 0;
      ARC_CNT  : UInt4 := 0;
   end record
      with Size => 8;
   for OBSERVE_TX_Register use record
      PLOS_CNT at 0 range 4 .. 7;
      ARC_CNT  at 0 range 0 .. 3;
   end record;

   type RPD_Register is new Boolean
      with Size => 8;

   subtype RX_ADDR_P0_Register is UInt40;
   subtype RX_ADDR_P1_Register is UInt40;
   subtype RX_ADDR_P2_Register is UInt8;
   subtype RX_ADDR_P3_Register is UInt8;
   subtype RX_ADDR_P4_Register is UInt8;
   subtype RX_ADDR_P5_Register is UInt8;

   subtype TX_ADDR_Register is UInt40;

   type RX_PW_Register is new UInt6
      with Size => 8;

   type FIFO_STATUS_Register is record
      TX_REUSE : Boolean := False;
      TX_FULL  : Boolean := False;
      TX_EMPTY : Boolean := True;
      RX_FULL  : Boolean := False;
      RX_EMPTY : Boolean := True;
   end record
      with Size => 8;
   for FIFO_STATUS_Register use record
      TX_REUSE at 0 range 6 .. 6;
      TX_FULL  at 0 range 5 .. 5;
      TX_EMPTY at 0 range 4 .. 4;
      RX_FULL  at 0 range 1 .. 1;
      RX_EMPTY at 0 range 0 .. 0;
   end record;

   type DYNPD_Register is array (Data_Pipe) of Boolean
      with Component_Size => 1, Size => 8;

   type FEATURE_Register is record
      EN_DPL     : Boolean := False;
      EN_ACK_PAY : Boolean := False;
      EN_DYN_ACK : Boolean := False;
   end record
      with Size => 8;
   for FEATURE_Register use record
      EN_DPL     at 0 range 2 .. 2;
      EN_ACK_PAY at 0 range 1 .. 1;
      EN_DYN_ACK at 0 range 0 .. 0;
   end record;

   subtype STATUS_Register is NRF24L01_IO.STATUS_Register;

   package CONFIG is new NRF24L01_IO.Register
      (16#00#, CONFIG_Register);
   package EN_AA is new NRF24L01_IO.Register
      (16#01#, EN_AA_Register);
   package EN_RXADDR is new NRF24L01_IO.Register
      (16#02#, EN_RXADDR_Register);
   package SETUP_AW is new NRF24L01_IO.Register
      (16#03#, SETUP_AW_Register);
   package SETUP_RETR is new NRF24L01_IO.Register
      (16#04#, SETUP_RETR_Register);
   package RF_CH is new NRF24L01_IO.Register
      (16#05#, RF_CH_Register);
   package RF_SETUP is new NRF24L01_IO.Register
      (16#06#, RF_SETUP_Register);
   package STATUS is new NRF24L01_IO.Register
      (16#07#, STATUS_Register);
   package OBSERVE_TX is new NRF24L01_IO.Register
      (16#08#, OBSERVE_TX_Register);
   package RPD is new NRF24L01_IO.Register
      (16#09#, RPD_Register);
   package RX_ADDR_P0 is new NRF24L01_IO.Register
      (16#0A#, RX_ADDR_P0_Register);
   package RX_ADDR_P1 is new NRF24L01_IO.Register
      (16#0B#, RX_ADDR_P1_Register);
   package RX_ADDR_P2 is new NRF24L01_IO.Register
      (16#0C#, RX_ADDR_P2_Register);
   package RX_ADDR_P3 is new NRF24L01_IO.Register
      (16#0D#, RX_ADDR_P3_Register);
   package RX_ADDR_P4 is new NRF24L01_IO.Register
      (16#0E#, RX_ADDR_P4_Register);
   package RX_ADDR_P5 is new NRF24L01_IO.Register
      (16#0F#, RX_ADDR_P5_Register);
   package TX_ADDR is new NRF24L01_IO.Register
      (16#10#, TX_ADDR_Register);
   package RX_PW_P0 is new NRF24L01_IO.Register
      (16#11#, RX_PW_Register);
   package RX_PW_P1 is new NRF24L01_IO.Register
      (16#12#, RX_PW_Register);
   package RX_PW_P2 is new NRF24L01_IO.Register
      (16#13#, RX_PW_Register);
   package RX_PW_P3 is new NRF24L01_IO.Register
      (16#14#, RX_PW_Register);
   package RX_PW_P4 is new NRF24L01_IO.Register
      (16#15#, RX_PW_Register);
   package RX_PW_P5 is new NRF24L01_IO.Register
      (16#16#, RX_PW_Register);
   package FIFO_STATUS is new NRF24L01_IO.Register
      (16#17#, FIFO_STATUS_Register);
   package DYNPD is new NRF24L01_IO.Register
      (16#1C#, DYNPD_Register);
   package FEATURE is new NRF24L01_IO.Register
      (16#1D#, FEATURE_Register);

end NRF24L01_Registers;

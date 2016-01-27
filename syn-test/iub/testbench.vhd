--==============================================================================
-- KTH Royal Institute of Technology Stockholm
-- Testbench for IUB to Minion connections
--==============================================================================
--
-- author: Theodor Stana (stana@kth.se)
--
-- date of creation: 2016-01-26
--
-- version: 1.0
--
-- description:
--
--==============================================================================
-- GNU LESSER GENERAL PUBLIC LICENSE
--==============================================================================
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--==============================================================================
-- last changes:
--    2016-01-26   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testbench is
end entity testbench;

architecture arch of testbench is
  --===========================================================================
  -- Components
  --===========================================================================
  component minion is
    port
    (
      ---------------------------------------------------------------------------
      -- Clock
      ---------------------------------------------------------------------------
      clk_i          : in  std_logic;

      ---------------------------------------------------------------------------
      -- IUB side ports
      ---------------------------------------------------------------------------
      -- Data shift ports
      iub_shift_i    : in  std_logic;
      iub_read_i     : in  std_logic;
      iub_data_i     : in  std_logic;

      iub_shift_dbg_o : out std_logic;
      iub_read_dbg_o : out std_logic;
      iub_data_dbg_o : out std_logic;

  --    -- Temperature MUX-ing ports
  --    temp_i         : in  std_logic_vector(15 downto 0);
  --    iub_temp_o     : out std_logic;

  --    -- PPS port IUB -> DIO
  --    iub_pps_i      : in  std_logic;
  --    pps_o          : out std_logic;
  --
  --    -- Backup signals to the IUB
  --    iub_bkp1_o      : out std_logic;
  --    iub_bkp2_o      : out std_logic;
  --    iub_bkp3_o      : out std_logic;

      ---------------------------------------------------------------------------
      -- Ports to the power board
      ---------------------------------------------------------------------------
      fadc_pwr_en_o  : out std_logic_vector(5 downto 0);
      pmt_pwr_en_o   : out std_logic_vector(5 downto 0);
      dio_pwr_en_o   : out std_logic;
      spwrt_pwr_en_o : out std_logic;
      sp3_pwr_en_o   : out std_logic
    );
  end component minion;

  --===========================================================================
  -- Constants
  --===========================================================================
  constant c_clk_per   : time := 20 ns;
  constant c_shift_per : time := 200 ns;

  --===========================================================================
  -- Signals
  --===========================================================================
  -- Testbench side ports
  signal clk       : std_logic := '0';

  signal data      : std_logic;
  signal shift     : std_logic := '0';
  signal read      : std_logic;

  signal serialize : std_logic := '0';
  signal wait_cnt  : natural := 0;
  signal offset_en : std_logic := '0';
  signal offset    : natural := 0;

  signal bit_count : natural := 0;
  signal data_vect : std_logic_vector(0 to 39);

  signal temp_sel  : unsigned(0 to 3) := "1111";
  signal fadc      : std_logic_vector(1 to 6);
  signal pmt       : std_logic_vector(1 to 6);
  signal dio       : std_logic;
  signal spwrt     : std_logic;
  signal sp3       : std_logic;

  -- DUT side ports
  signal fadc_pwr  : std_logic_vector(5 downto 0);
  signal pmt_pwr   : std_logic_vector(5 downto 0);
  signal dio_pwr   : std_logic;
  signal spwrt_pwr : std_logic;
  signal sp3_pwr   : std_logic;

begin

  --===========================================================================
  -- What to enable -- PLAY HERE!!!
  --===========================================================================
  fadc(1) <= '0';
  fadc(2) <= '1';
  fadc(3) <= '0';
  fadc(4) <= '0';
  fadc(5) <= '1';
  fadc(6) <= '1';

  pmt(1)  <= '1';
  pmt(2)  <= '0';
  pmt(3)  <= '0';
  pmt(4)  <= '0';
  pmt(5)  <= '1';
  pmt(6)  <= '0';

  dio     <= '1';
  spwrt   <= '1';
  sp3     <= '1';

  --===========================================================================
  -- Mimic IUB behaviour
  --===========================================================================
  -- Assign corresponding parts of the data vector
  data_vect( 0 to  3) <= std_logic_vector(temp_sel);
  data_vect( 4 to  9) <= fadc;
  data_vect(10 to 15) <= (others => '0');
  data_vect(16)       <= '0';
  data_vect(17)       <= sp3;
  data_vect(18)       <= spwrt;
  data_vect(19)       <= '0';
  data_vect(20 to 25) <= pmt;
  data_vect(26 to 31) <= (others => '0');
  data_vect(32)       <= dio;
  data_vect(33 to 39) <= (others => '0');

  -- IUB shift clock
  p_shift : process
  begin
    shift <= not shift;
    wait for c_shift_per/2;
  end process p_shift;

  -- Serialize data
  p_stim_p2s : process (shift)
  begin
    if rising_edge(shift) then

      case serialize is

        when '0' =>
          wait_cnt <= wait_cnt + 1;
          if (wait_cnt = 19 + offset) then
            wait_cnt <= 0;
            read <= '1';
            serialize <= '1';

            temp_sel <= temp_sel-1;

            offset_en <= not offset_en;
            if (offset_en = '1') then
              offset <= 31;
            else
              offset <= 0;
            end if;
          end if;

        when '1' =>
          read <= '0';
          bit_count <= bit_count + 1;
          if (bit_count = 39) then
            bit_count <= 0;
            serialize <= '0';
          end if;

        when others =>
          serialize <= '0';

      end case;

    end if;
  end process p_stim_p2s;

  -- Assign data output asynchronously
  data <= data_vect(bit_count) when (serialize = '1') else '0';

  --===========================================================================
  -- 50 MHz clock on the minion
  --===========================================================================
  p_clk : process
  begin
    clk <= not clk;
    wait for c_clk_per/2;
  end process p_clk;

  --===========================================================================
  -- DUT
  --===========================================================================
  cmp_dut : minion
    port map
    (
      ---------------------------------------------------------------------------
      -- Clock
      ---------------------------------------------------------------------------
      clk_i           => clk,

      ---------------------------------------------------------------------------
      -- IUB side ports
      ---------------------------------------------------------------------------
      -- Data shift ports
      iub_shift_i     => shift,
      iub_read_i      => read,
      iub_data_i      => data,

      iub_shift_dbg_o => open,
      iub_read_dbg_o  => open,
      iub_data_dbg_o  => open,

      ---------------------------------------------------------------------------
      -- Ports to the power board
      ---------------------------------------------------------------------------
      fadc_pwr_en_o  => fadc_pwr,
      pmt_pwr_en_o   => pmt_pwr,
      dio_pwr_en_o   => dio_pwr,
      spwrt_pwr_en_o => spwrt_pwr,
      sp3_pwr_en_o   => sp3_pwr
    );

end architecture arch;

--==============================================================================
-- KTH Royal Institute of Technology Stockholm
-- PoGO+ minion board gateware
--==============================================================================
--
-- author: Theodor Stana (stana@kth.se)
--
-- date of creation: 2015-10-06
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
--    2015-10-06   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minion is
  generic
  (
    g_num_fadc_boards  : natural := 6
  );
  port
  (
    ---------------------------------------------------------------------------
    -- Ports for one-hit veto
    ---------------------------------------------------------------------------
    clk_i          : in  std_logic;
    trig_i         : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    hit_i          : in  std_logic_vector(g_num_fadc_boards-1 downto 0);

    ---------------------------------------------------------------------------
    -- Ports for temperature multiplexing to the IUB
    ---------------------------------------------------------------------------
    clk_shift_i    : in  std_logic;
    clk_read_i     : in  std_logic;
    data_i         : in  std_logic;
    temp_i         : in  std_logic_vector(18 downto 0);

    fadc_pwr_en_o  : out std_logic_vector(g_num_fadc_boards-1 downto 0);
    pmt_pwr_en_o   : out std_logic_vector(g_num_fadc_boards-1 downto 0);
    dio_pwr_en_o   : out std_logic;
    spwrt_pwr_en_o : out std_logic;
    sp3_pwr_en_o   : out std_logic;
    temp_o         : out std_logic
  );
end entity minion;

architecture behav of minion is

  --===========================================================================
  -- Functions
  --===========================================================================

  --===========================================================================
  -- Types
  --===========================================================================

  --===========================================================================
  -- Signals
  --===========================================================================
  signal sh_reg         : std_logic_vector(39 downto 0);
  signal data_from_iub  : std_logic_vector(39 downto 0);

  signal temp_sel       : std_logic_vector( 4 downto 0);

  signal fadc_pwr_en    : std_logic_vector(g_num_fadc_boards-1 downto 0);
  signal pmt_pwr_en     : std_logic_vector(g_num_fadc_boards-1 downto 0);
  signal dio_pwr_en     : std_logic;
  signal spwrt_pwr_en   : std_logic;
  signal sp3_pwr_en     : std_logic;

--=============================================================================
-- architecture begin
--=============================================================================
begin

  --===========================================================================
  -- Temperature sensor MUXing
  --===========================================================================
  -- Shift and data storage registers, controlled by signals from the IUB
  p_shift_reg : process(clk_shift_i)
  begin
    if rising_edge(clk_shift_i) then
      sh_reg <= data_i & sh_reg(39 downto 1);
    end if;
  end process p_shift_reg;

  p_data_reg : process(clk_read_i)
  begin
    if rising_edge(clk_read_i) then
      data_from_iub <= sh_reg;
    end if;
  end process p_data_reg;

  -- Split IUB data into needed fields
  temp_sel        <= data_from_iub( 4 downto  0);
  fadc_pwr_en     <= data_from_iub( 5+(g_num_fadc_boards-1) downto  5);
  dio_pwr_en      <= data_from_iub(17);
  spwrt_pwr_en    <= data_from_iub(18);
  sp3_pwr_en      <= data_from_iub(20);
  pmt_pwr_en      <= data_from_iub(21+(g_num_fadc_boards-1) downto 21);

  -- Assign outputs (power enable signals + temp. sensor MUX)
  fadc_pwr_en_o   <= fadc_pwr_en;
  pmt_pwr_en_o    <= pmt_pwr_en;
  dio_pwr_en_o    <= dio_pwr_en;
  spwrt_pwr_en_o  <= spwrt_pwr_en;
  sp3_pwr_en_o    <= sp3_pwr_en;
  temp_o          <= temp_i(to_integer(unsigned(temp_sel)));

end architecture behav;
--=============================================================================
-- architecture end
--=============================================================================

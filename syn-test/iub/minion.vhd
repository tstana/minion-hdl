--==============================================================================
-- KTH Royal Institute of Technology Stockholm
-- PoGO+ minion board gateware
--==============================================================================
--
-- author: Theodor Stana (stana@kth.se)
--
-- date of creation: 2016-01-22
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
--    2016-01-22   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minion is
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
end entity minion;

architecture behav of minion is

  --===========================================================================
  -- Signals
  --===========================================================================
  signal iub_shift_d0       : std_logic;
  signal iub_shift_fedge_p0 : std_logic;
  signal read_dly           : std_logic_vector(39 downto 0);
  signal sh_reg             : std_logic_vector(39 downto 0);
  signal data_from_iub      : std_logic_vector(39 downto 0);

  signal temp_sel           : std_logic_vector( 3 downto 0);

  signal pps_out            : std_logic;

  signal fadc_pwr_en        : std_logic_vector(5 downto 0);
  signal pmt_pwr_en         : std_logic_vector(5 downto 0);
  signal dio_pwr_en         : std_logic;
  signal spwrt_pwr_en       : std_logic;
  signal sp3_pwr_en         : std_logic;

--=============================================================================
-- architecture begin
--=============================================================================
begin

  --===========================================================================
  -- DEBUG -- remove!!!
  --===========================================================================
  iub_shift_dbg_o <= iub_shift_i;
  iub_read_dbg_o  <= iub_read_i;
  iub_data_dbg_o  <= iub_data_i;

  --===========================================================================
  -- IUB control
  --===========================================================================
  -- Detect falling edge on shift input
  p_shift_fall_edge : process (clk_i)
  begin
    if rising_edge(clk_i) then
      iub_shift_d0       <= iub_shift_i;
      iub_shift_fedge_p0 <= iub_shift_d0 and (not iub_shift_i);
    end if;
  end process p_shift_fall_edge;

  -- Shift and data storage registers, controlled by signals from the IUB
  p_shift_reg : process(clk_i)
  begin
    if rising_edge(clk_i) then
      if (iub_shift_fedge_p0 = '1') then
        sh_reg   <= sh_reg(38 downto 0) & iub_data_i;
        read_dly <= read_dly(38 downto 0) & iub_read_i;
        if (read_dly(39) = '1') then
          for i in 0 to 39 loop
            data_from_iub(i) <= sh_reg(39-i);
          end loop;
        end if;
      end if;
    end if;
  end process p_shift_reg;

  -- Split IUB data into relevant fields
  temp_sel     <= data_from_iub( 3 downto  0);
  fadc_pwr_en  <= data_from_iub( 9 downto  4);
  sp3_pwr_en   <= data_from_iub(17);
  spwrt_pwr_en <= data_from_iub(18);
  dio_pwr_en   <= data_from_iub(32);
  pmt_pwr_en   <= data_from_iub(25 downto 20);

--  -- Temperature MUX output assignment
--  -- NOTE: Should not be clocked, since temp sensor output is duty-cycle-encoded
--  iub_temp_o <= temp_i(to_integer(unsigned(temp_sel)));
--
--  -- Backup connections to IUB
--  iub_bkp1_o <= '0';
--  iub_bkp2_o <= '0';
--  iub_bkp3_o <= '0';
--
--  -- Forward PPS from IUB to DIO
--  pps_out <= iub_pps_i;

  --===========================================================================
  -- Power enable outputs assignment
  --===========================================================================
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      fadc_pwr_en_o  <= fadc_pwr_en;
      pmt_pwr_en_o   <= pmt_pwr_en;
      dio_pwr_en_o   <= dio_pwr_en;
      spwrt_pwr_en_o <= spwrt_pwr_en;
      sp3_pwr_en_o   <= sp3_pwr_en;
    end if;
  end process;

end architecture behav;
--=============================================================================
-- architecture end
--=============================================================================

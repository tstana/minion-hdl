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

    temp_i          : in  std_logic_vector(15 downto 0);
    iub_temp_o      : out std_logic;
    iub_bkp1_o      : out std_logic;
    iub_bkp2_o      : out std_logic;
    iub_bkp3_o      : out std_logic;

    ---------------------------------------------------------------------------
    -- FADC & DIO side ports
    ---------------------------------------------------------------------------
    -- FADC -> DIO
    fadc1_i         : in  std_logic_vector(7 downto 0);
    fadc2_i         : in  std_logic_vector(7 downto 0);
    fadc3_i         : in  std_logic_vector(7 downto 0);
    fadc4_i         : in  std_logic_vector(7 downto 0);
    fadc5_i         : in  std_logic_vector(7 downto 0);
    fadc6_i         : in  std_logic_vector(7 downto 0);

    dio_o           : out std_logic_vector(7 downto 0);

    -- DIO -> FADC
    dio_i           : in  std_logic_vector(7 downto 0);

    fadc1_o         : out std_logic_vector(7 downto 0);
    fadc2_o         : out std_logic_vector(7 downto 0);
    fadc3_o         : out std_logic_vector(7 downto 0);
    fadc4_o         : out std_logic_vector(7 downto 0);
    fadc5_o         : out std_logic_vector(7 downto 0);
    fadc6_o         : out std_logic_vector(7 downto 0);

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
  -- Constants
  --===========================================================================
  -- Reset period in clock cycles: 5000000*20ns = 100 ms
  constant c_reset_per      : natural := 4999999;

  --===========================================================================
  -- Signals
  --===========================================================================
  signal reset              : std_logic := '0';
  signal reset_count_dis    : std_logic := '0';
  signal reset_count        : unsigned(22 downto 0) := (others => '0');

  signal iub_shift_d0       : std_logic;
  signal iub_shift_d1       : std_logic;
  signal iub_shift_d2       : std_logic;
  signal iub_shift_fedge_p0 : std_logic;
  signal read_dly           : std_logic_vector(39 downto 0);
  signal sh_reg             : std_logic_vector(39 downto 0);
  signal data_from_iub      : std_logic_vector(39 downto 0);

  signal temp_sel           : std_logic_vector(3 downto 0);
  signal fadc_pwr_en        : std_logic_vector(5 downto 0);
  signal pmt_pwr_en         : std_logic_vector(5 downto 0);
  signal dio_pwr_en         : std_logic;
  signal spwrt_pwr_en       : std_logic;
  signal sp3_pwr_en         : std_logic;

  signal trig_in            : std_logic_vector(5 downto 0);
  signal writing_in         : std_logic_vector(5 downto 0);
  signal wd_in              : std_logic_vector(5 downto 0);
  signal ud_in              : std_logic_vector(5 downto 0);
  signal hit_in             : std_logic_vector(5 downto 0);

  signal do_write_in        : std_logic;
  signal pseudo_pps_in      : std_logic;
  signal stop_in            : std_logic;

  signal trig_or            : std_logic;
  signal writing_or         : std_logic;
  signal writing_and        : std_logic;
  signal wd_or              : std_logic;
  signal ud_or              : std_logic;

  signal do_write_out       : std_logic_vector(5 downto 0);
  signal pseudo_pps_out     : std_logic_vector(5 downto 0);
  signal stop_out           : std_logic_vector(5 downto 0);

--=============================================================================
-- architecture begin
--=============================================================================
begin

  --===========================================================================
  -- DEBUG
  --===========================================================================
  iub_shift_dbg_o <= iub_shift_i;
  iub_read_dbg_o  <= iub_read_i;
  iub_data_dbg_o  <= iub_data_i;

  --===========================================================================
  -- Power-on reset
  --===========================================================================
  p_reset : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if (reset_count_dis = '0') then
        reset_count <= reset_count + 1;
        reset       <= '1';
        if (reset_count = c_reset_per-1) then
          reset_count_dis <= '1';
          reset           <= '0';
        end if;
      end if;
    end if;
  end process p_reset;

  --===========================================================================
  -- IUB control
  --===========================================================================
  -- Detect falling edge on shift input
  p_shift_fall_edge : process (reset, clk_i)
  begin
    if (reset = '1') then
      iub_shift_d0       <= '0';
      iub_shift_d1       <= '0';
      iub_shift_d2       <= '0';
      iub_shift_fedge_p0 <= '0';
    elsif rising_edge(clk_i) then
      iub_shift_d0       <= iub_shift_i;
      iub_shift_d1       <= iub_shift_d0;
      iub_shift_d2       <= iub_shift_d1;
      iub_shift_fedge_p0 <= iub_shift_d2 and (not iub_shift_d1);
    end if;
  end process p_shift_fall_edge;

  -- Shift and data storage registers, controlled by signals from the IUB
  p_shift_reg : process(reset, clk_i)
  begin
    if (reset = '1') then
      sh_reg        <= (others => '0');
      read_dly      <= (others => '0');
      data_from_iub <= (others => '0');
    elsif rising_edge(clk_i) then
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

  -- Temperature MUX output assignment
  -- NOTE: Should not be clocked, since temp sensor output is duty-cycle-encoded
  iub_temp_o <= temp_i(to_integer(unsigned(temp_sel)));

 --===========================================================================
  -- LVDS inputs to internal signal assignments
  --===========================================================================
  -- Inputs from FADCs
  trig_in(0)    <= fadc1_i(0);
  trig_in(1)    <= fadc2_i(0);
  trig_in(2)    <= fadc3_i(0);
  trig_in(3)    <= fadc4_i(0);
  trig_in(4)    <= fadc5_i(0);
  trig_in(5)    <= fadc6_i(0);

  writing_in(0) <= fadc1_i(1);
  writing_in(1) <= fadc2_i(1);
  writing_in(2) <= fadc3_i(1);
  writing_in(3) <= fadc4_i(1);
  writing_in(4) <= fadc5_i(1);
  writing_in(5) <= fadc6_i(1);

  wd_in(0)      <= fadc1_i(2);
  wd_in(1)      <= fadc2_i(2);
  wd_in(2)      <= fadc3_i(2);
  wd_in(3)      <= fadc4_i(2);
  wd_in(4)      <= fadc5_i(2);
  wd_in(5)      <= fadc6_i(2);

  ud_in(0)      <= fadc1_i(3);
  ud_in(1)      <= fadc2_i(3);
  ud_in(2)      <= fadc3_i(3);
  ud_in(3)      <= fadc4_i(3);
  ud_in(4)      <= fadc5_i(3);
  ud_in(5)      <= fadc6_i(3);

  hit_in(0)     <= fadc1_i(4);
  hit_in(1)     <= fadc2_i(4);
  hit_in(2)     <= fadc3_i(4);
  hit_in(3)     <= fadc4_i(4);
  hit_in(4)     <= fadc5_i(4);
  hit_in(5)     <= fadc6_i(4);

  -- Inputs from DIO
  pseudo_pps_in <= dio_i(0);
  do_write_in   <= dio_i(1);
  stop_in       <= dio_i(2);

  --===========================================================================
  -- FADC -> DIO outputs
  --===========================================================================
  -- Trigger OR
  trig_or <= trig_in(0) or trig_in(1); --'1' when (trig_in /= (trig_in'range => '0')) else '0';

  -- FADCs writing
  writing_and <= writing_in(0) and writing_in(1); -- '1' when (writing_in  = (writing_in'range => '1')) else '0';
  writing_or  <= writing_in(0) or writing_in(1);  -- '1' when (writing_in /= (writing_in'range => '0')) else '0';

  -- Upper Discrimination (UD) & Waveform Discrimination (WD) veto signals
--  ud_or  <= '1' when (ud_in /= (ud_in'range => '0')) else '0';
--  wd_or  <= '1' when (wd_in /= (wd_in'range => '0')) else '0';

  -- Assign the outputs
  process (reset, clk_i)
  begin
    if (reset = '1') then
      dio_o <= (others => '0');
    elsif rising_edge(clk_i) then
      dio_o(0) <= trig_or;
      dio_o(1) <= writing_or;
      dio_o(2) <= '0'; -- writing_and;
      dio_o(3) <= '0'; -- ud_or;
      dio_o(4) <= '0'; -- wd_or;
      dio_o(5) <= '0'; -- pps_out;
      dio_o(6) <= '0';
      dio_o(7) <= '0';
    end if;
  end process;

  --===========================================================================
  -- DIO -> FADC outputs
  --===========================================================================
  -- Fan-out to ports
  process (reset, clk_i)
  begin
    if (reset = '1') then
      do_write_out   <= (others => '0');
      pseudo_pps_out <= (others => '0');
      stop_out       <= (others => '0');
    elsif rising_edge(clk_i) then
      do_write_out   <= (do_write_out'range   => do_write_in);
      pseudo_pps_out <= (pseudo_pps_out'range => pseudo_pps_in);
      stop_out       <= (stop_out'range       => stop_in);
    end if;
  end process;

  -- Port assignments
  fadc1_o(0) <= pseudo_pps_out(0);
  fadc1_o(1) <= do_write_out(0);
  fadc1_o(2) <= stop_out(0);
  fadc1_o(3) <= '0';
  fadc1_o(4) <= '0';
  fadc1_o(5) <= '0';
  fadc1_o(6) <= '0';
  fadc1_o(7) <= '0';

  fadc2_o(0) <= pseudo_pps_out(1);
  fadc2_o(1) <= do_write_out(1);
  fadc2_o(2) <= stop_out(1);
  fadc2_o(3) <= '0';
  fadc2_o(4) <= '0';
  fadc2_o(5) <= '0';
  fadc2_o(6) <= '0';
  fadc2_o(7) <= '0';

  fadc3_o(0) <= pseudo_pps_out(2);
  fadc3_o(1) <= do_write_out(2);
  fadc3_o(2) <= stop_out(2);
  fadc3_o(3) <= '0';
  fadc3_o(4) <= '0';
  fadc3_o(5) <= '0';
  fadc3_o(6) <= '0';
  fadc3_o(7) <= '0';

  fadc4_o(0) <= pseudo_pps_out(3);
  fadc4_o(1) <= do_write_out(3);
  fadc4_o(2) <= stop_out(3);
  fadc4_o(3) <= '0';
  fadc4_o(4) <= '0';
  fadc4_o(5) <= '0';
  fadc4_o(6) <= '0';
  fadc4_o(7) <= '0';

  fadc5_o(0) <= pseudo_pps_out(4);
  fadc5_o(1) <= do_write_out(4);
  fadc5_o(2) <= stop_out(4);
  fadc5_o(3) <= '0';
  fadc5_o(4) <= '0';
  fadc5_o(5) <= '0';
  fadc5_o(6) <= '0';
  fadc5_o(7) <= '0';

  fadc6_o(0) <= pseudo_pps_out(5);
  fadc6_o(1) <= do_write_out(5);
  fadc6_o(2) <= stop_out(5);
  fadc6_o(3) <= '0';
  fadc6_o(4) <= '0';
  fadc6_o(5) <= '0';
  fadc6_o(6) <= '0';
  fadc6_o(7) <= '0';

  --===========================================================================
  -- Power enable outputs assignment
  --===========================================================================
  process (reset, clk_i)
  begin
    if (reset = '1') then
      fadc_pwr_en_o  <= (others => '0');
      pmt_pwr_en_o   <= (others => '0');
      dio_pwr_en_o   <= '0';
      spwrt_pwr_en_o <= '0';
      sp3_pwr_en_o   <= '0';
    elsif rising_edge(clk_i) then
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

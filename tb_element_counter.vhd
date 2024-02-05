library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use std.textio.all;
  use ieee.std_logic_textio.all;

entity TB_ELEMENT_COUNTER is -- keine Schnittstellen
end entity TB_ELEMENT_COUNTER;

architecture TESTBENCH of TB_ELEMENT_COUNTER is

  component ELEMENT_COUNTER is
    generic (
      G_IMAGE_HEIGHT : integer := 96;
      G_IMAGE_WIDTH  : integer := 128;
      G_MIN_SIZE     : integer := 16;
      G_MAX_SIZE     : integer := 500
    );
    port (
      I_CLOCK             : in    std_logic;
      I_RESET_N           : in    std_logic;

      I_START             : in    std_logic;
      I_PIXEL             : in    std_logic_vector(7 downto 0);
      I_PIXEL_VALID       : in    std_logic;

      O_COUNT_VALID       : out   std_logic;
      O_COUNT             : out   std_logic_vector(11 downto 0);

      O_PIXEL_VALID       : out   std_logic;
      O_PIXEL             : out   std_logic_vector(15 downto 0)
    );
  end component;

  procedure discard_separator (
    variable line_pointer : inout line
  ) is

    variable dump : string(1 to 1);

  begin

    read(line_pointer, dump);

  end procedure;

  procedure get_integer (
    variable line_pointer : inout line;
    variable int_out      : out integer) is

    variable v_int_out           : integer;
    variable v_separator_discard : string(1 to 1);

  begin

    read(line_pointer, v_int_out);
    int_out := v_int_out;
    discard_separator(line_pointer);

  end procedure;

  procedure get_integer (
    variable line_pointer : inout line;
    signal int_out        : out std_logic_vector) is

    variable v_int_out           : integer;
    variable v_separator_discard : string(1 to 1);

  begin

    read(line_pointer, v_int_out);
    int_out <= std_logic_vector(to_unsigned(v_int_out, int_out'length));
    discard_separator(line_pointer);

  end procedure;

  -- Ports in Richtung nutzende Komponente
  signal clk_tb_s                           : std_logic;
  signal w_reset_n                          : std_logic;
  signal w_write_enable                     : std_logic;
  signal w_pixel                            : std_logic_vector(7 downto 0);
  signal w_ready                            : std_logic;

  signal r_write_enable                     : std_logic := '0';
  signal r_write_enable_delay               : std_logic := '0';
  signal r_pixel                            : std_logic_vector(7 downto 0);

  constant c_image_width                    : integer := 128;
  constant c_image_height                   : integer := 96;

  type out_memory is ARRAY (0 to c_image_height - 1, 0 to c_image_width - 1) of integer;

  signal result_image                       : out_memory;

  constant c_filename_image                 : string := "C:\Users\Schul\Entwicklung\QuartusProjekte\Abschlussprojekt\Element_Counter\zaehltest.csv";
  constant c_filename_out                   : string := "C:\Users\Schul\Entwicklung\QuartusProjekte\Abschlussprojekt\Element_Counter\segmented.csv";

  file   fptr                               : text;
  signal test_int                           : integer;
  -- signal test_string : string(1 to 1);

  signal r_line_opened                      : std_logic := '0';

  signal test_string                        : string(1 to 1);

  type t_states is (CLOSE_FILE, TRANSMIT_IMAGE, LOAD_COUNT, LOAD_SEGMENTED_IMAGE, FINISHED);

  signal w_next_state                       : t_states;
  signal r_current_state                    : t_states := CLOSE_FILE;

  -- Current column and of the first image to load
  -- Row 0 <= x < block_size
  -- Values get reset after each load
  signal r_image_load_col                   : integer := 0;
  signal r_image_load_row                   : integer := 0;

  signal r_start                            : std_logic;
  signal r_in_pixel_valid                   : std_logic;

  signal w_count_valid                      : std_logic;
  signal w_count                            : std_logic_vector(11 downto 0);

  signal w_segment_pixel_valid              : std_logic;
  signal w_segment_pixel                    : std_logic_vector(15 downto 0);

  signal r_image_row_pointer                : integer := 0;

  signal r_count                            : std_logic_vector(11 downto 0);

begin

  DUT : ELEMENT_COUNTER
    port map (
      I_CLOCK   => clk_tb_s,
      I_RESET_N => w_reset_n,

      I_START       => r_start,
      I_PIXEL       => r_pixel,
      I_PIXEL_VALID => r_in_pixel_valid,

      O_COUNT_VALID => w_count_valid,
      O_COUNT       => w_count,

      O_PIXEL_VALID => w_segment_pixel_valid,
      O_PIXEL       => w_segment_pixel
    );

  P_CLK : process is

  begin

    clk_tb_s <= '1';
    wait for 5 ns;
    clk_tb_s <= '0';
    wait for 5 ns;

  end process P_CLK;

  PROC_STATE_OUT : process (r_current_state, r_image_load_col, r_image_load_row, w_ready, w_count_valid, w_segment_pixel_valid) is

    variable v_col_count_image1 : integer := 0;
    variable v_row_count_image1 : integer := 0;

    variable v_col_count_image2 : integer := 0;
    variable v_row_count_image2 : integer := 0;
    variable v_current_cycle    : integer := 1;

    variable v_fstatus  : file_open_status;
    variable v_line_out : line;

    file image1 : text open read_mode is c_filename_image;

    variable v_current_image1_line : line;

  begin

    case r_current_state is

      when CLOSE_FILE =>
        w_next_state <= TRANSMIT_IMAGE;

        w_write_enable <= '0';
        w_pixel        <= (others => '0');

      -- Übertrage BLOCK_SIZE Zeilen von Bild 1
      when TRANSMIT_IMAGE =>

        if (r_image_load_col = c_image_width - 1 and r_image_load_row = c_image_height - 1) then
          w_next_state <= LOAD_COUNT;
        else
          w_next_state <= TRANSMIT_IMAGE;
        end if;

      -- Lade Disparitätspixel
      -- Danach lade entweder nächste 2 Bilder oder speichere Datei.
      when LOAD_COUNT =>

        if (w_count_valid = '1') then
          w_next_state <= LOAD_SEGMENTED_IMAGE;
        else
          w_next_state <= LOAD_COUNT;
        end if;

      when LOAD_SEGMENTED_IMAGE =>

        if (r_image_load_col = c_image_width - 1 and r_image_load_row < c_image_height - 1 and w_segment_pixel_valid = '1') then
          w_next_state <= LOAD_SEGMENTED_IMAGE;
        elsif (r_image_load_col = c_image_width - 1 and r_image_load_row = c_image_height - 1 and w_segment_pixel_valid = '1') then
          w_next_state <= FINISHED;
        else
          w_next_state <= LOAD_SEGMENTED_IMAGE;
        end if;

      when FINISHED =>

    end case;

  end process PROC_STATE_OUT;

  PROC_STATE_FF : process (w_reset_n, clk_tb_s) is
  begin

  end process PROC_STATE_FF;

  PROC_FILE_HANDLER_IN : process (w_reset_n, clk_tb_s) is

    variable fstatus  : file_open_status;
    variable line_out : line;

    file image1 : text open read_mode is c_filename_image;

    variable current_image1_line : line;
    variable current_image2_line : line;

  begin

    if (w_reset_n = '0') then
      r_pixel         <= (others => '0');
      r_current_state <= CLOSE_FILE;
    elsif (rising_edge(clk_tb_s)) then
      r_current_state <= w_next_state;

      case r_current_state is

        when CLOSE_FILE =>
          file_close(fptr);
          file_open(fstatus, fptr, c_filename_out, write_mode);
          readline(image1, current_image1_line);
          r_start          <= '1';
          r_in_pixel_valid <= '0';

        when TRANSMIT_IMAGE =>

          r_start          <= '1';
          r_in_pixel_valid <= '1';

          get_integer(current_image1_line, r_pixel);

          if (r_image_load_col < c_image_width - 1) then
            r_image_load_col <= r_image_load_col + 1;
          else
            r_image_load_col <= 0;
          end if;

          if (r_image_load_col = c_image_width - 1) then
            if (r_image_load_row < c_image_height - 1) then
              if (r_image_row_pointer < c_image_height - 1) then
                readline(image1, current_image1_line);
              end if;
              r_image_load_row    <= r_image_load_row + 1;
              r_image_row_pointer <= r_image_row_pointer + 1;
            else
              r_image_load_row <= 0;
            end if;
          end if;

        when LOAD_COUNT =>

          if (w_count_valid = '1') then
            r_count <= w_count;
          end if;

          r_in_pixel_valid <= '0';
          r_start          <= '0';

        when LOAD_SEGMENTED_IMAGE =>
          if (w_segment_pixel_valid = '1') then
            result_image(r_image_load_row, r_image_load_col) <= to_integer(unsigned(w_segment_pixel));

            write(line_out, to_integer(unsigned(w_segment_pixel)));

            if (r_image_load_col < c_image_width - 1) then
              write(line_out, string'(","));
            else
              writeline(fptr, line_out);
            end if;

            if (r_image_load_col < c_image_width - 1) then
              r_image_load_col <= r_image_load_col + 1;
            else
              r_image_load_col <= 0;
            end if;

            if (r_image_load_col = c_image_width - 1) then
              r_image_load_row <= r_image_load_row + 1;
            end if;
          end if;

        -- r_image_load_col <= 0;
        -- r_image_load_row <= 0;

        when FINISHED =>
          file_close(fptr);

      end case;

    end if;

  end process PROC_FILE_HANDLER_IN;

  PROC_WRITE_ENABLE_DELAY : process (w_reset_n, clk_tb_s) is
  begin

    if (w_reset_n = '0') then
      r_write_enable <= '0';
    elsif (rising_edge(clk_tb_s)) then
      r_write_enable_delay <= w_write_enable;
      r_write_enable       <= r_write_enable_delay;
    end if;

  end process PROC_WRITE_ENABLE_DELAY;

  w_reset_n <= '1';

end architecture TESTBENCH;

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity ELEMENT_COUNTER is
  generic (
    G_IMAGE_HEIGHT     : integer := 48;
    G_IMAGE_WIDTH      : integer := 64;
    G_MIN_SIZE         : integer := 16;
    G_MAX_SIZE         : integer := 500;
    G_BINARY_THRESHOLD : integer := 0
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
end entity ELEMENT_COUNTER;

architecture RTL of ELEMENT_COUNTER is

  constant c_pixel_amount                          : integer := G_IMAGE_HEIGHT * G_IMAGE_WIDTH;

  type t_states is (IDLE, STORE_IMAGE, LOAD_NEIGHBOURS, LABEL_FIRST_NEIGHBOURS, STORE_TEMP_LABEL, LOAD_TEMP_LABEL, LOAD_EQUIVALENCY, STORE_REAL_LABEL, LOAD_REGION, LOAD_CURRENT_REGION_SIZE, INCREMENT_REGION_SIZE, COUNT_REGIONS, OUTPUT_SEGMENTED_IMAGE);

  signal r_current_state                           : t_states;
  signal w_next_state                              : t_states;

  signal r_segmentation_ram_write_pointer          : std_logic_vector(13 downto 0) := (others => '0');

  signal r_segmentation_ram_out_pointer            : std_logic_vector(13 downto 0) := (others => '0');

  signal r_segmentation_ram_read_pointer_b         : std_logic_vector(13 downto 0);

  signal w_segmentation_ram_q_a                    : std_logic_vector(15 downto 0);
  signal w_segmentation_ram_q_b                    : std_logic_vector(15 downto 0);
  signal w_segmentation_ram_data_a                 : std_logic_vector(15 downto 0);
  signal w_segmentation_ram_data_b                 : std_logic_vector(15 downto 0);
  signal w_segmentation_ram_address_a              : std_logic_vector(13 downto 0);
  signal w_segmentation_ram_address_b              : std_logic_vector(13 downto 0);
  signal w_segmentation_ram_write_enable_a         : std_logic;
  signal w_segmentation_ram_write_enable_b         : std_logic;

  signal w_equivalency_table_address_a             : std_logic_vector(11 downto 0);
  signal w_equivalency_table_address_b             : std_logic_vector(11 downto 0);
  signal w_equivalency_table_data_a                : std_logic_vector(15 downto 0);
  signal w_equivalency_table_data_b                : std_logic_vector(15 downto 0);
  signal w_equivalency_table_q_a                   : std_logic_vector(15 downto 0);
  signal w_equivalency_table_q_b                   : std_logic_vector(15 downto 0);
  signal w_equivalency_table_write_enable_a        : std_logic;
  signal w_equivalency_table_write_enable_b        : std_logic;

  signal w_size_table_address_a                    : std_logic_vector(11 downto 0);
  signal w_size_table_address_b                    : std_logic_vector(11 downto 0);
  signal w_size_table_data_a                       : std_logic_vector(15 downto 0);
  signal w_size_table_data_b                       : std_logic_vector(15 downto 0);
  signal w_size_table_q_a                          : std_logic_vector(15 downto 0);
  signal w_size_table_q_b                          : std_logic_vector(15 downto 0);
  signal w_size_table_write_enable_a               : std_logic;
  signal w_size_table_write_enable_b               : std_logic;

  signal r_previous_row_unlabelled_pixel           : integer := 0;
  signal r_previous_col_unlabelled_pixel           : integer := 0;

  signal r_current_row_unlabelled_pixel            : integer := 1;
  signal r_current_col_unlabelled_pixel            : integer := 1;

  signal r_next_col_unlabelled_pixel               : integer := 2;
  signal r_next_row_unlabelled_pixel               : integer := 1;

  signal w_ram_address_unlabelled                  : std_logic_vector(13 downto 0);
  signal w_ram_address_next_unlabelled             : std_logic_vector(13 downto 0);
  signal w_ram_address_upper                       : std_logic_vector(13 downto 0);
  signal w_ram_address_left                        : std_logic_vector(13 downto 0);

  signal r_upper_pixel                             : std_logic_vector(15 downto 0);
  signal r_left_pixel                              : std_logic_vector(15 downto 0);
  signal r_unlabelled_pixel                        : std_logic_vector(15 downto 0);

  signal r_new_label                               : std_logic_vector(15 downto 0);

  -- Prefetch the very first label to prevent
  -- the need of adding an additional state
  signal r_first_temp_label                        : std_logic_vector(15 downto 0);

  -- 0 for a, 1 for b
  signal r_equivalency_a_or_b                      : std_logic := '1';

  signal r_next_region                             : unsigned(15 downto 0) := "0000000000000010";
  signal w_new_region_found                        : std_logic;

  signal r_size_out_pointer                        : std_logic_vector(11 downto 0) := (others => '0');

  signal r_element_count                           : integer range 0 to 4095 := 0;

  signal r_last_labelled_pixel : std_logic_vector(15 downto 0) := (others => '0');

  alias a_left_pixel                               : std_logic_vector(15 downto 0) is w_segmentation_ram_q_a;
  alias a_upper_pixel                              : std_logic_vector(15 downto 0) is w_segmentation_ram_q_b;
  alias a_unlabelled_pixel                         : std_logic_vector(15 downto 0) is w_segmentation_ram_q_b;

  component SEGMENTATION_RAM is
    port (
      ADDRESS_A    : in    std_logic_vector(13 downto 0);
      ADDRESS_B    : in    std_logic_vector(13 downto 0);
      CLOCK        : in    std_logic  := '1';
      DATA_A       : in    std_logic_vector(15 downto 0);
      DATA_B       : in    std_logic_vector(15 downto 0);
      WREN_A       : in    std_logic  := '0';
      WREN_B       : in    std_logic  := '0';
      Q_A          : out   std_logic_vector(15 downto 0);
      Q_B          : out   std_logic_vector(15 downto 0)
    );
  end component segmentation_ram;

  component EQUIVALENCY_TABLE is
    port (
      ADDRESS_A    : in    std_logic_vector(11 downto 0);
      ADDRESS_B    : in    std_logic_vector(11 downto 0);
      CLOCK        : in    std_logic  := '1';
      DATA_A       : in    std_logic_vector(15 downto 0);
      DATA_B       : in    std_logic_vector(15 downto 0);
      WREN_A       : in    std_logic  := '0';
      WREN_B       : in    std_logic  := '0';
      Q_A          : out   std_logic_vector(15 downto 0);
      Q_B          : out   std_logic_vector(15 downto 0)
    );
  end component equivalency_table;

begin

  SEG_RAM : SEGMENTATION_RAM
    port map (
      CLOCK     => I_CLOCK,
      DATA_A    => w_segmentation_ram_data_a,
      DATA_B    => w_segmentation_ram_data_b,
      ADDRESS_A => w_segmentation_ram_address_a,
      ADDRESS_B => w_segmentation_ram_address_b,
      WREN_A    => w_segmentation_ram_write_enable_a,
      WREN_B    => w_segmentation_ram_write_enable_b,
      Q_A       => w_segmentation_ram_q_a,
      Q_B       => w_segmentation_ram_q_b
    );

  EQ_TABLE : EQUIVALENCY_TABLE
    port map (
      CLOCK     => I_CLOCK,
      DATA_A    => w_equivalency_table_data_a,
      DATA_B    => w_equivalency_table_data_b,
      ADDRESS_A => w_equivalency_table_address_a,
      ADDRESS_B => w_equivalency_table_address_b,
      WREN_A    => w_equivalency_table_write_enable_a,
      WREN_B    => w_equivalency_table_write_enable_b,
      Q_A       => w_equivalency_table_q_a,
      Q_B       => w_equivalency_table_q_b
    );

  SIZE_TABLE : EQUIVALENCY_TABLE
    port map (
      CLOCK     => I_CLOCK,
      DATA_A    => w_size_table_data_a,
      DATA_B    => w_size_table_data_b,
      ADDRESS_A => w_size_table_address_a,
      ADDRESS_B => w_size_table_address_b,
      WREN_A    => w_size_table_write_enable_a,
      WREN_B    => w_size_table_write_enable_b,
      Q_A       => w_size_table_q_a,
      Q_B       => w_size_table_q_b
    );

  w_ram_address_unlabelled      <= std_logic_vector(to_unsigned(r_current_col_unlabelled_pixel + r_current_row_unlabelled_pixel * G_IMAGE_WIDTH, w_ram_address_unlabelled'length));
  w_ram_address_next_unlabelled <= std_logic_vector(to_unsigned(r_next_col_unlabelled_pixel + r_next_row_unlabelled_pixel * G_IMAGE_WIDTH, w_ram_address_unlabelled'length));
  w_ram_address_left            <= std_logic_vector(to_unsigned(r_previous_col_unlabelled_pixel + r_current_row_unlabelled_pixel * G_IMAGE_WIDTH, w_ram_address_left'length));
  w_ram_address_upper           <= std_logic_vector(to_unsigned(r_current_col_unlabelled_pixel + r_previous_row_unlabelled_pixel * G_IMAGE_WIDTH, w_ram_address_upper'length));

  PROC_ASYNC : process (w_size_table_q_a, r_size_out_pointer, r_segmentation_ram_out_pointer, r_first_temp_label, I_PIXEL, r_unlabelled_pixel, w_ram_address_next_unlabelled, w_segmentation_ram_q_a, w_equivalency_table_q_a, a_upper_pixel, a_left_pixel, I_START, w_new_region_found, r_next_region, w_ram_address_upper, w_ram_address_left, w_ram_address_unlabelled, I_PIXEL_VALID, r_current_state, r_current_col_unlabelled_pixel, r_previous_col_unlabelled_pixel, r_current_row_unlabelled_pixel, r_previous_row_unlabelled_pixel, r_segmentation_ram_write_pointer) is
  begin

    -- w_ram_address_left <=

    case r_current_state is

      when IDLE =>

        if (I_START = '1') then
          w_next_state <= STORE_IMAGE;
        else
          w_next_state <= IDLE;
        end if;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_address_a      <= (others => '0');
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when STORE_IMAGE =>

        if (to_integer(unsigned(r_segmentation_ram_write_pointer)) = c_pixel_amount - 1) then
          w_next_state <= LOAD_NEIGHBOURS;
        else
          w_next_state <= STORE_IMAGE;
        end if;

        if (to_integer(unsigned(I_PIXEL)) > G_BINARY_THRESHOLD) then
          w_segmentation_ram_data_a <= std_logic_vector(to_unsigned(1, w_segmentation_ram_data_a'length));
        else
          w_segmentation_ram_data_a <= (others => '0');
        end if;

        w_segmentation_ram_write_enable_a <= I_PIXEL_VALID;
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_a      <= r_segmentation_ram_write_pointer;
        w_segmentation_ram_address_b      <= w_ram_address_unlabelled;

        w_equivalency_table_write_enable_a <= '1';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');
        w_equivalency_table_address_a      <= r_segmentation_ram_write_pointer(11 downto 0);
        w_equivalency_table_address_b      <= (others => '0');

        w_size_table_write_enable_a <= '1';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= r_segmentation_ram_write_pointer(11 downto 0);
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      -- FIRST ROUND
      when LOAD_NEIGHBOURS =>

        if (r_current_row_unlabelled_pixel = 1) then
          w_next_state <= STORE_TEMP_LABEL;
        else
          w_next_state <= STORE_TEMP_LABEL;
        end if;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_address_a      <= w_ram_address_left;
        w_segmentation_ram_address_b      <= w_ram_address_upper;
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when LABEL_FIRST_NEIGHBOURS =>
        w_next_state <= STORE_TEMP_LABEL;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_address_a      <= (others => '0');
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when STORE_TEMP_LABEL =>

        if (r_current_col_unlabelled_pixel = G_IMAGE_WIDTH - 1 and r_current_row_unlabelled_pixel = G_IMAGE_HEIGHT - 1) then
          w_next_state <= LOAD_TEMP_LABEL;
        else
          w_next_state <= LOAD_NEIGHBOURS;
        end if;

        if (unsigned(r_unlabelled_pixel) > 0) then
          w_segmentation_ram_write_enable_a <= '1';
        else
          w_segmentation_ram_write_enable_a <= '0';
        end if;

        w_segmentation_ram_address_a      <= w_ram_address_unlabelled;
        w_segmentation_ram_address_b      <= w_ram_address_next_unlabelled;
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_data_b         <= (others => '0');

        -- Prefetch the first equivalency. It's needed as soon as the
        -- STORE_REAL_LABEL state is entered.

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= r_first_temp_label(11 downto 0);
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

        if (unsigned(r_unlabelled_pixel) > 0) then
          -- new region found
          if (unsigned(a_upper_pixel) < 2 and to_integer(unsigned(a_left_pixel)) < 2) then
            w_new_region_found                 <= '1';
            w_segmentation_ram_data_a          <= std_logic_vector(r_next_region);
            w_equivalency_table_data_a         <= std_logic_vector(r_next_region);
            w_equivalency_table_address_a      <= std_logic_vector(r_next_region(11 downto 0));
            w_equivalency_table_write_enable_a <= '1';
            w_equivalency_table_write_enable_b <= '0';

          -- current unlabelled pixel is part of exactly one other region
          elsif ((unsigned(a_upper_pixel) < 2 and unsigned(a_left_pixel) > 1)
                 or (unsigned(a_upper_pixel) > 1 and unsigned(a_left_pixel) < 2)
                 or (unsigned(a_upper_pixel) = unsigned(a_left_pixel) and unsigned(a_left_pixel) > 1)) then
            if (unsigned(a_upper_pixel) > 1) then
              w_segmentation_ram_data_a <= a_upper_pixel;
            else
              w_segmentation_ram_data_a <= a_left_pixel;
            end if;

            w_equivalency_table_write_enable_a <= '0';
            w_equivalency_table_write_enable_b <= '0';
            w_new_region_found                 <= '0';

          -- current unlabelled pixel connects two regions
          else
            w_new_region_found <= '0';
            if (a_upper_pixel < a_left_pixel) then
              w_segmentation_ram_data_a          <= a_upper_pixel;
              w_equivalency_table_data_a         <= a_upper_pixel;
              w_equivalency_table_address_a      <= a_left_pixel(11 downto 0);
              w_equivalency_table_write_enable_a <= '1';
            else
              w_segmentation_ram_data_a          <= a_left_pixel;
              w_equivalency_table_data_a         <= a_left_pixel;
              w_equivalency_table_address_a      <= a_upper_pixel(11 downto 0);
              w_equivalency_table_write_enable_a <= '1';
            end if;
          end if;
        else
          w_equivalency_table_data_a <= (others => '0');
          w_segmentation_ram_data_a  <= (others => '0');
        end if;

      -- SECOND ROUND
      when LOAD_TEMP_LABEL =>
        w_next_state <= LOAD_EQUIVALENCY;

        w_segmentation_ram_address_a      <= w_ram_address_unlabelled;
        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_b      <= (others => '0');

        w_equivalency_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_data_a         <= (others => '0');

        w_size_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_size_table_write_enable_a <= '0';
        w_size_table_data_a         <= (others => '0');
        w_size_table_write_enable_b <= '0';
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_segmentation_ram_write_enable_b <= '0';

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when LOAD_EQUIVALENCY =>

        w_next_state <= STORE_REAL_LABEL;

        w_segmentation_ram_address_a      <= w_ram_address_unlabelled;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_segmentation_ram_write_enable_b <= '0';

        w_equivalency_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_data_a         <= (others => '0');

        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_data_b         <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');

        w_size_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_size_table_write_enable_a <= '0';
        w_size_table_data_a         <= (others => '0');
        w_size_table_write_enable_b <= '0';
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when STORE_REAL_LABEL =>

        if (r_current_col_unlabelled_pixel = G_IMAGE_WIDTH - 1 and r_current_row_unlabelled_pixel = G_IMAGE_HEIGHT - 1) then
          w_next_state <= LOAD_REGION;
        else
          w_next_state <= LOAD_TEMP_LABEL;
        end if;

        if (unsigned(w_segmentation_ram_q_a) > 1) then
          w_segmentation_ram_write_enable_a <= '1';
        else
          w_segmentation_ram_write_enable_a <= '0';
        end if;
        
        w_segmentation_ram_data_a         <= w_equivalency_table_q_a;
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_a      <= w_ram_address_unlabelled;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_write_enable_b <= '0';
        
        w_size_table_write_enable_a       <= '0';
        w_size_table_data_a         <= std_logic_vector(unsigned(w_equivalency_table_q_a) + 1);
        w_size_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);

        w_size_table_write_enable_b <= '0';
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when LOAD_REGION =>
        w_next_state <= LOAD_CURRENT_REGION_SIZE;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_a      <= r_segmentation_ram_out_pointer;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_write_enable_b <= '0';
        
        w_size_table_write_enable_a       <= '0';
        w_size_table_data_a         <= (others => '0');
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_write_enable_b <= '0';
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when LOAD_CURRENT_REGION_SIZE =>

        w_next_state <= INCREMENT_REGION_SIZE;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_a      <= r_segmentation_ram_out_pointer;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_write_enable_b <= '0';
        
        w_size_table_write_enable_a       <= '0';
        w_size_table_data_a         <= (others => '0');
        w_size_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_size_table_address_b      <= (others => '0');
        w_size_table_write_enable_b <= '0';
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when INCREMENT_REGION_SIZE =>

        if(unsigned(r_segmentation_ram_out_pointer) = c_pixel_amount - 1) then
          w_next_state <= COUNT_REGIONS;
        else
          w_next_state <= LOAD_REGION;
        end if;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');
        w_segmentation_ram_address_a      <= r_segmentation_ram_out_pointer;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_write_enable_b <= '0';
        
        
        if(unsigned(w_segmentation_ram_q_a(11 downto 0)) > 1) then
          w_size_table_write_enable_b <= '1';
        else
          w_size_table_write_enable_b <= '0';
        end if;
        
        
        w_size_table_write_enable_a       <= '0';
        w_size_table_data_a         <= (others => '0');
        w_size_table_address_a      <= w_segmentation_ram_q_a(11 downto 0);
        w_size_table_address_b      <= w_segmentation_ram_q_a(11 downto 0);
        w_size_table_data_b         <= std_logic_vector(unsigned(w_size_table_q_a) + 1);

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when COUNT_REGIONS =>

        if (unsigned(r_size_out_pointer) = 4095) then
          w_next_state <= OUTPUT_SEGMENTED_IMAGE;
        else
          w_next_state <= COUNT_REGIONS;
        end if;

        w_size_table_address_b      <= r_size_out_pointer;
        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_segmentation_ram_address_a      <= r_segmentation_ram_out_pointer;
        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_address_a      <= (others => '0');
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

      when OUTPUT_SEGMENTED_IMAGE =>

        if (unsigned(r_segmentation_ram_out_pointer) = c_pixel_amount) then
          w_next_state <= IDLE;
        else
          w_next_state <= OUTPUT_SEGMENTED_IMAGE;
        end if;

        w_segmentation_ram_write_enable_a <= '0';
        w_segmentation_ram_write_enable_b <= '0';
        w_segmentation_ram_address_a      <= r_segmentation_ram_out_pointer;
        w_segmentation_ram_address_b      <= (others => '0');
        w_segmentation_ram_data_a         <= (others => '0');
        w_segmentation_ram_data_b         <= (others => '0');

        w_equivalency_table_write_enable_a <= '0';
        w_equivalency_table_write_enable_b <= '0';
        w_equivalency_table_address_a      <= (others => '0');
        w_equivalency_table_address_b      <= (others => '0');
        w_equivalency_table_data_a         <= (others => '0');
        w_equivalency_table_data_b         <= (others => '0');

        w_size_table_write_enable_a <= '0';
        w_size_table_write_enable_b <= '0';
        w_size_table_address_a      <= (others => '0');
        w_size_table_address_b      <= (others => '0');
        w_size_table_data_a         <= (others => '0');
        w_size_table_data_b         <= (others => '0');

        w_new_region_found <= '0';

    end case;

  end process PROC_ASYNC;

  PROC_SYNC : process (I_CLOCK, I_RESET_N) is
  begin

    if (I_RESET_N = '0') then
      r_current_state <= IDLE;
      r_segmentation_ram_write_pointer <= (others => '0');
      r_unlabelled_pixel               <= (others => '0');
      r_next_region                    <= (others => '0');
      r_current_col_unlabelled_pixel   <= 1;
      r_current_row_unlabelled_pixel   <= 1;
      r_next_col_unlabelled_pixel      <= 2;
      r_next_row_unlabelled_pixel      <= 1;
      r_size_out_pointer               <= (others => '0');
      r_segmentation_ram_out_pointer   <= (others => '0');
      r_element_count                  <= 0;
    elsif (rising_edge(I_CLOCK)) then
      r_current_state <= w_next_state;

      case r_current_state is

        when IDLE =>
        r_segmentation_ram_write_pointer <= (others => '0');
        r_unlabelled_pixel               <= (others => '0');
        r_next_region                    <= (others => '0');
        r_current_col_unlabelled_pixel   <= 1;
        r_current_row_unlabelled_pixel   <= 1;
        r_next_col_unlabelled_pixel      <= 2;
        r_next_row_unlabelled_pixel      <= 1;
        r_size_out_pointer               <= (others => '0');
        r_segmentation_ram_out_pointer   <= (others => '0');
        r_element_count                  <= 0;

        when STORE_IMAGE =>

          if (I_PIXEL_VALID = '1') then
            if (to_integer(unsigned(r_segmentation_ram_write_pointer)) < c_pixel_amount - 1) then
              r_segmentation_ram_write_pointer <= std_logic_vector(unsigned(r_segmentation_ram_write_pointer) + 1);
            end if;
          end if;

          -- preload first unlabelled pixel
          if (w_next_state = LOAD_NEIGHBOURS) then
            r_unlabelled_pixel <= w_segmentation_ram_q_b;
          end if;

        when LOAD_NEIGHBOURS =>
          r_unlabelled_pixel <= w_segmentation_ram_q_b;

        when LABEL_FIRST_NEIGHBOURS =>

        when STORE_TEMP_LABEL =>
          if (w_new_region_found = '1') then
            r_next_region <= r_next_region + 1;
          end if;

          if (r_current_col_unlabelled_pixel < G_IMAGE_WIDTH - 1) then
            r_previous_col_unlabelled_pixel <= r_current_col_unlabelled_pixel;
            r_current_col_unlabelled_pixel  <= r_current_col_unlabelled_pixel + 1;
          else
            r_previous_col_unlabelled_pixel <= G_IMAGE_WIDTH - 1;
            r_current_col_unlabelled_pixel  <= 1;
          end if;

          if (r_current_col_unlabelled_pixel = G_IMAGE_WIDTH - 1) then
            if (r_current_row_unlabelled_pixel < G_IMAGE_HEIGHT - 1) then
              r_previous_row_unlabelled_pixel <= r_current_row_unlabelled_pixel;
              r_current_row_unlabelled_pixel  <= r_current_row_unlabelled_pixel + 1;
            else
              r_previous_row_unlabelled_pixel <= G_IMAGE_WIDTH - 1;
              r_current_row_unlabelled_pixel  <= 1;
            end if;
          end if;

          if (r_next_col_unlabelled_pixel < G_IMAGE_WIDTH - 1) then
            r_next_col_unlabelled_pixel <= r_next_col_unlabelled_pixel + 1;
          else
            r_next_col_unlabelled_pixel <= 1;
          end if;

          if (r_next_col_unlabelled_pixel = G_IMAGE_WIDTH - 1) then
            if (r_next_row_unlabelled_pixel < G_IMAGE_HEIGHT - 1) then
              r_next_row_unlabelled_pixel <= r_next_row_unlabelled_pixel + 1;
            else
              r_next_row_unlabelled_pixel <= 1;
            end if;
          end if;

        when LOAD_TEMP_LABEL =>

        when LOAD_EQUIVALENCY =>

        when STORE_REAL_LABEL =>

          if (r_current_col_unlabelled_pixel < G_IMAGE_WIDTH - 1) then
            r_current_col_unlabelled_pixel <= r_current_col_unlabelled_pixel + 1;
          else
            r_current_col_unlabelled_pixel <= 0;
          end if;

          if (r_current_col_unlabelled_pixel = G_IMAGE_WIDTH - 1) then
            if (r_current_row_unlabelled_pixel < G_IMAGE_HEIGHT - 1) then
              r_current_row_unlabelled_pixel <= r_current_row_unlabelled_pixel + 1;
            else
              r_current_row_unlabelled_pixel <= 0;
            end if;
          end if;

          r_size_out_pointer <= std_logic_vector(to_unsigned(1, r_size_out_pointer'length));

        when LOAD_REGION =>
  
        when LOAD_CURRENT_REGION_SIZE =>
  
        when INCREMENT_REGION_SIZE =>

          if(unsigned(r_segmentation_ram_out_pointer) < c_pixel_amount - 1) then
            r_segmentation_ram_out_pointer <= std_logic_vector(unsigned(r_segmentation_ram_out_pointer) + 1);
          else
            r_segmentation_ram_out_pointer <= (others => '0');
          end if;

        when COUNT_REGIONS =>

          if (unsigned(r_size_out_pointer) < 4095) then
            r_size_out_pointer <= std_logic_vector(unsigned(r_size_out_pointer) + 1);
          else
            r_size_out_pointer <= (others => '0');
          end if;

          r_segmentation_ram_out_pointer <= std_logic_vector(to_unsigned(1, r_segmentation_ram_out_pointer'length));

          if (unsigned(w_size_table_q_b) > G_MIN_SIZE and unsigned(w_size_table_q_b) < G_MAX_SIZE) then
            r_element_count <= r_element_count + 1;
          end if;

        when OUTPUT_SEGMENTED_IMAGE =>

          if (unsigned(r_segmentation_ram_out_pointer) < c_pixel_amount) then
            r_segmentation_ram_out_pointer <= std_logic_vector(unsigned(r_segmentation_ram_out_pointer) + 1);
          else
            r_segmentation_ram_out_pointer <= (others => '0');
          end if;

      end case;

    end if;

  end process PROC_SYNC;

  O_COUNT       <= std_logic_vector(to_unsigned(r_element_count, O_COUNT'length));
  O_PIXEL       <= w_segmentation_ram_q_a;
  O_PIXEL_VALID <= '1' when r_current_state = OUTPUT_SEGMENTED_IMAGE else
                   '0';
  O_COUNT_VALID <= '1' when r_current_state = COUNT_REGIONS and unsigned(r_size_out_pointer) = 4095 else
                   '0';

end architecture RTL;

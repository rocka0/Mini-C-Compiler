################################################################################
# Set the Makeflags to have -j to enable parallel builds
################################################################################
MAKEFLAGS += -j

################################################################################
# Define variables for directories and files
################################################################################
BISON_DIR := /opt/homebrew/Cellar/bison/3.8.2/bin
BISON_BINARY := $(shell if [ -d $(BISON_DIR) ]; then echo $(BISON_DIR)/bison; else echo "bison"; fi)

INCLUDE_DIR := include
SRC_DIR := src
OUTPUT_DIR := bin

FLEX_SRC_FILE := $(SRC_DIR)/lexer/lexer.l
FLEX_CPP_FILE := $(OUTPUT_DIR)/lex.yy.cpp
FLEX_OUTPUT_FILE := flex.out

BISON_SRC_FILE := $(SRC_DIR)/parser/parser.y
BISON_CPP_FILE := $(OUTPUT_DIR)/y.tab.cpp
BISON_OUTPUT_FILE := bison.out

SYMBOL_TABLE_SRC_FILE := $(SRC_DIR)/symbol_table/symbol_table.cpp
AST_SRC_FILE := $(SRC_DIR)/ast/ast.cpp
QUAD_SRC_FILE := $(SRC_DIR)/quad/quad.cpp
MAIN_SRC_FILE := $(SRC_DIR)/main.cpp

FINAL_BINARY := $(OUTPUT_DIR)/mcc

################################################################################
# Define functions to remove files and directories
################################################################################
define remove_if_file_exists
	@if [ -e $(1) ]; then \
		rm $(1); \
	fi
endef

define remove_if_dir_exists
	@if [ -d $(1) ]; then \
		rm -rf $(1); \
	fi
endef

################################################################################
# Define the default target to build the final binary
################################################################################
all: $(FINAL_BINARY)

################################################################################
# Define targets and commands to build the lexer and parser cpp files
################################################################################
$(BISON_CPP_FILE): $(BISON_SRC_FILE)
	@mkdir -p $(OUTPUT_DIR)
	@echo "Compiling $(BISON_SRC_FILE) into $(BISON_CPP_FILE)"
	@$(BISON_BINARY) -d -o $(BISON_CPP_FILE) $(BISON_SRC_FILE)

$(FLEX_CPP_FILE): $(FLEX_SRC_FILE)
	@mkdir -p $(OUTPUT_DIR)
	@echo "Compiling $(FLEX_SRC_FILE) into $(FLEX_CPP_FILE)"
	@flex -o $(FLEX_CPP_FILE) $(FLEX_SRC_FILE)

################################################################################
# Define target and command to link source files into the final binary
################################################################################
$(FINAL_BINARY): $(FLEX_CPP_FILE) $(BISON_CPP_FILE) $(SYMBOL_TABLE_SRC_FILE) $(AST_SRC_FILE) $(QUAD_SRC_FILE) $(MAIN_SRC_FILE)
	@echo "Linking all source files into $(FINAL_BINARY)"
	@g++ -std=c++17 -Iinclude $(FLEX_CPP_FILE) $(BISON_CPP_FILE) $(SYMBOL_TABLE_SRC_FILE) $(AST_SRC_FILE) $(QUAD_SRC_FILE) $(MAIN_SRC_FILE) -o $(FINAL_BINARY)

################################################################################
# Define clean target to remove all generated files
################################################################################
clean:
	@echo "Cleaning generated files"
	$(call remove_if_dir_exists, $(OUTPUT_DIR))
	$(call remove_if_file_exists, $(FLEX_OUTPUT_FILE))
	$(call remove_if_file_exists, $(BISON_OUTPUT_FILE))

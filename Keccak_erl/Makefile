# Makefile for Keccak Native Erlang Implementation
# Provides build, test, and clean targets for the pure Erlang Keccak implementation

# Compiler and flags
ERLC = erlc
ERL = erl
EBIN_DIR = ebin
SRC_DIR = .

# Source files
SOURCES = $(wildcard $(SRC_DIR)/*.erl)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.erl=$(EBIN_DIR)/%.beam)

# Default target
all: compile

# Create ebin directory
$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

# Compile Erlang sources
compile: $(EBIN_DIR) $(OBJECTS)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl | $(EBIN_DIR)
	$(ERLC) -o $(EBIN_DIR) $<

# Run tests
test: compile
	@echo "Running Keccak Real Implementation Tests..."
	@$(ERL) -pa $(EBIN_DIR) -noshell -eval "keccak_real_test:run_tests(), halt()."

# Clean compiled files
clean:
	rm -rf $(EBIN_DIR)

# Rebuild everything
rebuild: clean all

# Run interactive shell with modules loaded
shell: compile
	@$(ERL) -pa $(EBIN_DIR)

# Documentation (if available)
docs:
	@echo "Generating documentation..."
	@$(ERL) -noshell -eval "edoc:application(keccak_native, \".\", []), halt()."

# Package for distribution
package: compile
	@echo "Creating package..."
	@mkdir -p keccak_native_package/ebin
	@cp -r $(EBIN_DIR)/* keccak_native_package/ebin/
	@cp -r $(SRC_DIR)/*.erl keccak_native_package/
	@tar -czf keccak_native_package.tar.gz keccak_native_package/
	@rm -rf keccak_native_package
	@echo "Package created: keccak_native_package.tar.gz"

.PHONY: all compile test clean rebuild shell docs package
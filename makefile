# Build all HTML files
all: generate-html

# Create directories and copy resources not really needed.
prepare-resources:
	# mkdir -p output/images
	# cp styles.css output/
	# cp -r images/* output/images/

# Generate HTML files
generate-html:
	cabal run

# Continue even if some targets fail
# .PHONY: all prepare-resources generate-html
.PHONY: all generate-html

clean:
	rm -rf output

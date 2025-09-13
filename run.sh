set -ex
cargo fmt
cargo test
RUST_BACKTRACE=1 cargo run -- --self-test
# RUST_BACKTRACE=1 cargo run -- ct_src/test_method.txt ct_src/example.txt -o output.txt
# RUST_BACKTRACE=1 cargo run -- ct_src/test_struct_fields.txt ct_src/example.txt -o output.txt
# RUST_BACKTRACE=1 cargo run -- ct_src/test_pretend_struct_fields.txt ct_src/example.txt -o output.txt
# RUST_BACKTRACE=1 cargo run -- ct_src/test_impl2.txt ct_src/example.txt -o output.txt
# RUST_BACKTRACE=1 cargo run -- ct_src/test_impld.txt ct_src/example.txt -o output.txt

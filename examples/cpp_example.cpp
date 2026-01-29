/**
 * @file cpp_example.cpp
 * @brief Example usage of the C++ wrapper for Keccak/SHA3 functions
 *
 * This example demonstrates various ways to use the C++ wrapper for Keccak/SHA3
 * hash functions, including both single-call and streaming APIs.
 *
 * @author Andy Wang
 * @date 2026
 */

#include "../src/keccak.hpp"
#include <iostream>
#include <vector>
#include <string>

int main() {
    std::cout << "╔═══════════════════════════════════════════════════════════╗" << std::endl;
    std::cout << "║              Keccak C++ Wrapper Examples                 ║" << std::endl;
    std::cout << "╚═══════════════════════════════════════════════════════════╝" << std::endl;

    // Example 1: Single-call SHA3-256
    std::cout << "\n=== Single-call SHA3-256 ===" << std::endl;
    std::string input1 = "Hello, World!";
    auto hash1 = keccak::sha3_256(input1);
    std::cout << "Input: \"" << input1 << "\"" << std::endl;
    std::cout << "SHA3-256: " << hash1.to_hex() << std::endl;

    // Example 2: Single-call SHA3-512
    std::cout << "\n=== Single-call SHA3-512 ===" << std::endl;
    std::string input2 = "Hello, World!";
    auto hash2 = keccak::sha3_512(input2);
    std::cout << "Input: \"" << input2 << "\"" << std::endl;
    std::cout << "SHA3-512: " << hash2.to_hex() << std::endl;

    // Example 3: Single-call SHA3-1024
    std::cout << "\n=== Single-call SHA3-1024 ===" << std::endl;
    std::string input3 = "Hello, World!";
    auto hash3 = keccak::sha3_1024(input3);
    std::cout << "Input: \"" << input3 << "\"" << std::endl;
    std::cout << "SHA3-1024 (first 64 bytes): " << hash3.to_hex().substr(0, 128) << std::endl;

    // Example 4: Custom Keccak hash
    std::cout << "\n=== Custom Keccak Hash ===" << std::endl;
    std::string input4 = "Hello, World!";
    auto hash4 = keccak::keccak(input4, 48);  // 48-byte output
    std::cout << "Input: \"" << input4 << "\"" << std::endl;
    std::cout << "Keccak-384 equivalent: " << hash4.to_hex() << std::endl;

    // Example 5: Streaming API SHA3-256
    std::cout << "\n=== Streaming API SHA3-256 ===" << std::endl;
    keccak::sha3_stream stream1(keccak::algorithm::sha3_256);
    stream1.update("Hello, ");
    stream1.update("World!");
    auto hash5 = stream1.finalize();
    std::cout << "Input: \"Hello, \" + \"World!\"" << std::endl;
    std::cout << "SHA3-256: " << hash5.to_hex() << std::endl;

    // Example 6: Streaming API with vector data
    std::cout << "\n=== Streaming API with Vector Data ===" << std::endl;
    std::vector<uint8_t> data_chunk1 = {'H', 'e', 'l'};
    std::vector<uint8_t> data_chunk2 = {'l', 'o'};
    keccak::sha3_stream stream2(keccak::algorithm::sha3_512);
    stream2.update(data_chunk1);
    stream2.update(data_chunk2);
    auto hash6 = stream2.finalize();
    std::cout << "Input: {'H','e','l'} + {'l','o'}" << std::endl;
    std::cout << "SHA3-512: " << hash6.to_hex() << std::endl;

    // Example 7: Hash comparison
    std::cout << "\n=== Hash Comparison ===" << std::endl;
    auto hash7 = keccak::sha3_256("Hello, World!");
    auto hash8 = keccak::sha3_256("Hello, World!");
    auto hash9 = keccak::sha3_256("Hello, world!");  // lowercase w
    
    std::cout << "Hash of \"Hello, World!\": " << hash7.to_hex().substr(0, 32) << "..." << std::endl;
    std::cout << "Hash of \"Hello, World!\": " << hash8.to_hex().substr(0, 32) << "..." << std::endl;
    std::cout << "Hash of \"Hello, world!\": " << hash9.to_hex().substr(0, 32) << "..." << std::endl;
    std::cout << "hash7 == hash8: " << (hash7 == hash8 ? "YES" : "NO") << std::endl;
    std::cout << "hash7 == hash9: " << (hash7 == hash9 ? "YES" : "NO") << std::endl;

    // Example 8: Error handling
    std::cout << "\n=== Error Handling Demo ===" << std::endl;
    try {
        keccak::sha3_stream bad_stream(keccak::algorithm::keccak_custom);  // Missing output length
    } catch (const std::exception& e) {
        std::cout << "Caught expected exception: " << e.what() << std::endl;
    }

    try {
        keccak::sha3_stream stream(keccak::algorithm::sha3_256);
        stream.update("test");
        auto result = stream.finalize();
        // Attempting to update after finalizing should throw
        stream.update("more data");
    } catch (const std::exception& e) {
        std::cout << "Caught expected exception after finalizing: " << e.what() << std::endl;
    }

    std::cout << "\n╔═══════════════════════════════════════════════════════════╗" << std::endl;
    std::cout << "║                 Examples Completed                       ║" << std::endl;
    std::cout << "╚═══════════════════════════════════════════════════════════╝" << std::endl;

    return 0;
}
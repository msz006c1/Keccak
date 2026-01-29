/**
 * @file keccak.hpp
 * @brief C++ wrapper for Keccak/SHA3 hash functions
 *
 * This header provides a C++ interface for the Keccak/SHA3 hash functions,
 * offering RAII-compliant classes and convenient methods for hashing.
 *
 * ## Features
 * - **RAII-compliant**: Automatic resource management
 * - **Exception-safe**: Proper exception handling
 * - **Modern C++**: Uses std::vector, std::string, and move semantics
 * - **Type-safe**: Strongly-typed hash results
 * - **Flexible**: Supports both single-call and streaming APIs
 *
 * ## Quick Start
 *
 * ### Single-call hashing:
 * ```cpp
 * #include "keccak.hpp"
 *
 * auto hash = keccak::sha3_256("Hello, World!");
 * std::cout << hash.to_hex() << std::endl;
 * ```
 *
 * ### Streaming hashing:
 * ```cpp
 * keccak::sha3_stream stream(keccak::algorithm::sha3_256);
 * stream.update("Hello, ");
 * stream.update("World!");
 * auto hash = stream.finalize();
 * std::cout << hash.to_hex() << std::endl;
 * ```
 *
 * @author Andy Wang
 * @date 2026
 * @version 1.0
 * @license Public Domain
 */

#ifndef KECCAK_HPP
#define KECCAK_HPP

#include "Keccak.h"
#include "sha3.h"
#include <vector>
#include <string>
#include <memory>
#include <stdexcept>
#include <sstream>
#include <iomanip>

namespace keccak {

/**
 * @brief Enum representing different hash algorithms
 */
enum class algorithm {
    keccak_custom,  ///< Custom Keccak with specified output length
    sha3_256,       ///< SHA3-256 (32-byte output)
    sha3_512,       ///< SHA3-512 (64-byte output)
    sha3_1024       ///< SHA3-1024 (128-byte output)
};

/**
 * @brief Class representing a hash result
 *
 * This class encapsulates the hash bytes and provides convenient methods
 * for conversion and comparison.
 */
class hash_result {
private:
    std::vector<uint8_t> bytes_;

public:
    /**
     * @brief Construct a hash result from raw bytes
     * @param bytes Vector of hash bytes
     */
    explicit hash_result(std::vector<uint8_t> bytes) : bytes_(std::move(bytes)) {}

    /**
     * @brief Get the raw hash bytes
     * @return Const reference to the byte vector
     */
    const std::vector<uint8_t>& bytes() const { return bytes_; }

    /**
     * @brief Get the size of the hash in bytes
     * @return Size of the hash
     */
    size_t size() const { return bytes_.size(); }

    /**
     * @brief Convert hash to hexadecimal string representation
     * @return Hexadecimal string representation of the hash
     */
    std::string to_hex() const {
        std::stringstream ss;
        ss << std::hex << std::setfill('0');
        for (auto byte : bytes_) {
            ss << std::setw(2) << static_cast<int>(byte);
        }
        return ss.str();
    }

    /**
     * @brief Convert hash to uppercase hexadecimal string representation
     * @return Uppercase hexadecimal string representation of the hash
     */
    std::string to_hex_upper() const {
        std::string hex = to_hex();
        for (auto& c : hex) {
            if (c >= 'a' && c <= 'f') {
                c = c - 'a' + 'A';
            }
        }
        return hex;
    }

    /**
     * @brief Equality operator
     * @param other Another hash_result to compare with
     * @return True if both hashes are equal
     */
    bool operator==(const hash_result& other) const {
        return bytes_ == other.bytes_;
    }

    /**
     * @brief Inequality operator
     * @param other Another hash_result to compare with
     * @return True if hashes are not equal
     */
    bool operator!=(const hash_result& other) const {
        return !(*this == other);
    }
};

/**
 * @brief Class for streaming hash computation
 *
 * This class provides a C++ interface for incremental hash computation.
 * It wraps the C streaming API with RAII compliance.
 */
class sha3_stream {
private:
    sha3_ctx ctx_;
    algorithm algo_;
    size_t output_length_;
    bool finalized_;

public:
    /**
     * @brief Constructor for streaming hash computation
     * @param algo The algorithm to use for hashing
     * @param custom_output_length Custom output length for keccak_custom algorithm
     */
    explicit sha3_stream(algorithm algo, size_t custom_output_length = 0) 
        : algo_(algo), finalized_(false) {
        
        switch (algo_) {
            case algorithm::sha3_256:
                output_length_ = 32;
                break;
            case algorithm::sha3_512:
                output_length_ = 64;
                break;
            case algorithm::sha3_1024:
                output_length_ = 128;
                break;
            case algorithm::keccak_custom:
                if (custom_output_length == 0) {
                    throw std::invalid_argument("Custom output length must be specified for keccak_custom");
                }
                output_length_ = custom_output_length;
                break;
        }
        
        sha3_init(&ctx_, output_length_);
    }

    /**
     * @brief Update the hash with new data
     * @param data Pointer to input data
     * @param length Length of input data in bytes
     * @throws std::runtime_error if stream is already finalized
     */
    void update(const uint8_t* data, size_t length) {
        if (finalized_) {
            throw std::runtime_error("Cannot update finalized stream");
        }
        sha3_update(&ctx_, data, length);
    }

    /**
     * @brief Update the hash with data from a vector
     * @param data Vector of bytes to hash
     * @throws std::runtime_error if stream is already finalized
     */
    void update(const std::vector<uint8_t>& data) {
        update(data.data(), data.size());
    }

    /**
     * @brief Update the hash with data from a string
     * @param str String to hash (will hash the raw bytes)
     * @throws std::runtime_error if stream is already finalized
     */
    void update(const std::string& str) {
        update(reinterpret_cast<const uint8_t*>(str.c_str()), str.length());
    }

    /**
     * @brief Finalize the hash computation and return the result
     * @return The computed hash result
     * @throws std::runtime_error if stream is already finalized
     */
    hash_result finalize() {
        if (finalized_) {
            throw std::runtime_error("Stream already finalized");
        }
        
        std::vector<uint8_t> output(output_length_);
        sha3_final(&ctx_, output.data(), output_length_);
        finalized_ = true;
        
        return hash_result(std::move(output));
    }

    /**
     * @brief Check if the stream has been finalized
     * @return True if the stream is finalized
     */
    bool is_finalized() const {
        return finalized_;
    }
    
    // Disable copying
    sha3_stream(const sha3_stream&) = delete;
    sha3_stream& operator=(const sha3_stream&) = delete;
    
    // Enable moving
    sha3_stream(sha3_stream&&) = default;
    sha3_stream& operator=(sha3_stream&&) = default;
};

/**
 * @brief Compute Keccak hash (single-call interface)
 * @param data Input data as byte vector
 * @param output_length Desired output length in bytes
 * @return The computed hash result
 */
inline hash_result keccak(const std::vector<uint8_t>& data, size_t output_length) {
    if (output_length == 0) {
        throw std::invalid_argument("Output length must be greater than 0");
    }

    std::vector<uint8_t> output(output_length);
    ::keccak_hash(output.data(), data.data(), data.size(), output_length);  // Use global scope operator to avoid recursion
    return hash_result(std::move(output));
}

/**
 * @brief Compute Keccak hash of string (single-call interface)
 * @param str Input string
 * @param output_length Desired output length in bytes
 * @return The computed hash result
 */
inline hash_result keccak(const std::string& str, size_t output_length) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return keccak(data, output_length);
}

/**
 * @brief Compute SHA3-256 hash (single-call interface)
 * @param data Input data as byte vector
 * @return The computed SHA3-256 hash result
 */
inline hash_result sha3_256(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(32);
    ::sha3_256(output.data(), data.data(), data.size());  // Use global scope operator to avoid recursion
    return hash_result(std::move(output));
}

/**
 * @brief Compute SHA3-256 hash of string (single-call interface)
 * @param str Input string
 * @return The computed SHA3-256 hash result
 */
inline hash_result sha3_256(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_256(data);
}

/**
 * @brief Compute SHA3-512 hash (single-call interface)
 * @param data Input data as byte vector
 * @return The computed SHA3-512 hash result
 */
inline hash_result sha3_512(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(64);
    ::sha3_512(output.data(), data.data(), data.size());  // Use global scope operator to avoid recursion
    return hash_result(std::move(output));
}

/**
 * @brief Compute SHA3-512 hash of string (single-call interface)
 * @param str Input string
 * @return The computed SHA3-512 hash result
 */
inline hash_result sha3_512(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_512(data);
}

/**
 * @brief Compute SHA3-1024 hash (single-call interface)
 * @param data Input data as byte vector
 * @return The computed SHA3-1024 hash result
 */
inline hash_result sha3_1024(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(128);
    ::sha3_1024(output.data(), data.data(), data.size());  // Use global scope operator to avoid recursion
    return hash_result(std::move(output));
}

/**
 * @brief Compute SHA3-1024 hash of string (single-call interface)
 * @param str Input string
 * @return The computed SHA3-1024 hash result
 */
inline hash_result sha3_1024(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_1024(data);
}

} // namespace keccak

#endif // KECCAK_HPP
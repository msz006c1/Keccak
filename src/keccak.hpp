/**
 * @file keccak.hpp
 * @brief C++ wrapper for Keccak/SHA3 hash functions.
 *
 * Provides RAII-compliant single-call and streaming APIs for Keccak/SHA3 hash functions.
 * Public domain.
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
 * @enum algorithm
 * @brief Enumerates the supported hash algorithms.
 */
enum class algorithm {
    keccak_custom,  /**< Custom Keccak algorithm with user-defined output length */
    sha3_256,       /**< SHA3-256 algorithm */
    sha3_512,       /**< SHA3-512 algorithm */
    sha3_1024       /**< SHA3-1024 algorithm */
};

/**
 * @class hash_result
 * @brief Represents the result of a hash computation with hex conversion capabilities.
 */
class hash_result {
private:
    std::vector<uint8_t> bytes_;

public:
    /**
     * @brief Constructs a hash result from a vector of bytes.
     * @param bytes The hash bytes
     */
    explicit hash_result(std::vector<uint8_t> bytes) : bytes_(std::move(bytes)) {}

    /**
     * @brief Gets the hash bytes.
     * @return A constant reference to the vector of bytes
     */
    const std::vector<uint8_t>& bytes() const { return bytes_; }

    /**
     * @brief Gets the size of the hash result.
     * @return The number of bytes in the hash
     */
    size_t size() const { return bytes_.size(); }

    /**
     * @brief Converts the hash to lowercase hexadecimal string.
     * @return The hexadecimal representation of the hash
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
     * @brief Converts the hash to uppercase hexadecimal string.
     * @return The uppercase hexadecimal representation of the hash
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
     * @brief Compares two hash results for equality.
     * @param other The other hash result to compare with
     * @return True if the hashes are equal, false otherwise
     */
    bool operator==(const hash_result& other) const { return bytes_ == other.bytes_; }

    /**
     * @brief Compares two hash results for inequality.
     * @param other The other hash result to compare with
     * @return True if the hashes are not equal, false otherwise
     */
    bool operator!=(const hash_result& other) const { return !(*this == other); }
};

/**
 * @class sha3_stream
 * @brief Streaming hash computation class that wraps the C streaming API.
 */
class sha3_stream {
private:
    sha3_ctx ctx_;              /**< The underlying C context */
    algorithm algo_;            /**< The algorithm being used */
    size_t output_length_;      /**< The expected output length */
    bool finalized_;            /**< Whether the stream has been finalized */

public:
    /**
     * @brief Constructs a streaming hash context.
     * @param algo The algorithm to use
     * @param custom_output_length The output length for custom Keccak (ignored for other algorithms)
     * @throws std::invalid_argument if custom_output_length is 0 for keccak_custom
     */
    explicit sha3_stream(algorithm algo, size_t custom_output_length = 0)
        : algo_(algo), finalized_(false) {

        switch (algo_) {
            case algorithm::sha3_256:  output_length_ = 32;  break;
            case algorithm::sha3_512:  output_length_ = 64;  break;
            case algorithm::sha3_1024: output_length_ = 128; break;
            case algorithm::keccak_custom:
                if (custom_output_length == 0)
                    throw std::invalid_argument("Custom output length must be specified for keccak_custom");
                output_length_ = custom_output_length;
                break;
        }

        sha3_init(&ctx_, output_length_);
    }

    /**
     * @brief Updates the hash with raw byte data.
     * @param data Pointer to the data to add
     * @param length Length of the data in bytes
     * @throws std::runtime_error if the stream is already finalized
     */
    void update(const uint8_t* data, size_t length) {
        if (finalized_) throw std::runtime_error("Cannot update finalized stream");
        sha3_update(&ctx_, data, length);
    }

    /**
     * @brief Updates the hash with data from a vector.
     * @param data Vector containing the data to add
     */
    void update(const std::vector<uint8_t>& data) {
        update(data.data(), data.size());
    }

    /**
     * @brief Updates the hash with data from a string.
     * @param str String containing the data to add
     */
    void update(const std::string& str) {
        update(reinterpret_cast<const uint8_t*>(str.c_str()), str.length());
    }

    /**
     * @brief Finalizes the hash computation and returns the result.
     * @return The computed hash result
     * @throws std::runtime_error if the stream is already finalized
     */
    hash_result finalize() {
        if (finalized_) throw std::runtime_error("Stream already finalized");
        std::vector<uint8_t> output(output_length_);
        sha3_final(&ctx_, output.data(), output_length_);
        finalized_ = true;
        return hash_result(std::move(output));
    }

    /**
     * @brief Checks if the stream has been finalized.
     * @return True if finalized, false otherwise
     */
    bool is_finalized() const { return finalized_; }

    /** @brief Copy constructor is deleted to prevent copying */
    sha3_stream(const sha3_stream&) = delete;
    /** @brief Copy assignment operator is deleted to prevent copying */
    sha3_stream& operator=(const sha3_stream&) = delete;
    /** @brief Move constructor is defaulted */
    sha3_stream(sha3_stream&&) = default;
    /** @brief Move assignment operator is defaulted */
    sha3_stream& operator=(sha3_stream&&) = default;
};

/** @name Single-call hash functions */
///@{

/**
 * @brief Computes a Keccak hash with the specified output length.
 * @param data The input data to hash
 * @param output_length The desired output length in bytes
 * @return The computed hash result
 * @throws std::invalid_argument if output_length is 0
 */
inline hash_result keccak(const std::vector<uint8_t>& data, size_t output_length) {
    if (output_length == 0) throw std::invalid_argument("Output length must be greater than 0");
    std::vector<uint8_t> output(output_length);
    ::keccak_hash(output.data(), data.data(), data.size(), output_length);
    return hash_result(std::move(output));
}

/**
 * @brief Computes a Keccak hash of a string with the specified output length.
 * @param str The input string to hash
 * @param output_length The desired output length in bytes
 * @return The computed hash result
 * @throws std::invalid_argument if output_length is 0
 */
inline hash_result keccak(const std::string& str, size_t output_length) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return keccak(data, output_length);
}

/**
 * @brief Computes a SHA3-256 hash of the input data.
 * @param data The input data to hash
 * @return The computed SHA3-256 hash result
 */
inline hash_result sha3_256(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(32);
    ::sha3_256(output.data(), data.data(), data.size());
    return hash_result(std::move(output));
}

/**
 * @brief Computes a SHA3-256 hash of the input string.
 * @param str The input string to hash
 * @return The computed SHA3-256 hash result
 */
inline hash_result sha3_256(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_256(data);
}

/**
 * @brief Computes a SHA3-512 hash of the input data.
 * @param data The input data to hash
 * @return The computed SHA3-512 hash result
 */
inline hash_result sha3_512(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(64);
    ::sha3_512(output.data(), data.data(), data.size());
    return hash_result(std::move(output));
}

/**
 * @brief Computes a SHA3-512 hash of the input string.
 * @param str The input string to hash
 * @return The computed SHA3-512 hash result
 */
inline hash_result sha3_512(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_512(data);
}

/**
 * @brief Computes a SHA3-1024 hash of the input data.
 * @param data The input data to hash
 * @return The computed SHA3-1024 hash result
 */
inline hash_result sha3_1024(const std::vector<uint8_t>& data) {
    std::vector<uint8_t> output(128);
    ::sha3_1024(output.data(), data.data(), data.size());
    return hash_result(std::move(output));
}

/**
 * @brief Computes a SHA3-1024 hash of the input string.
 * @param str The input string to hash
 * @return The computed SHA3-1024 hash result
 */
inline hash_result sha3_1024(const std::string& str) {
    std::vector<uint8_t> data(str.begin(), str.end());
    return sha3_1024(data);
}

///@}

} // namespace keccak

#endif // KECCAK_HPP

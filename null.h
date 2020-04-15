#pragma once
#include <iosfwd>

namespace Eval {
    // We define an inert type Null
    struct Null {
        friend std::ostream& operator<<(std::ostream& os, Null) { return os << "null"; }
        explicit operator bool() const { return false; }
    };

    template <typename T> Null operator+ (Null, T const&) { return {}; }
    template <typename T> Null operator- (Null, T const&) { return {}; }
    template <typename T> Null operator* (Null, T const&) { return {}; }
    template <typename T> Null operator/ (Null, T const&) { return {}; }
    template <typename T> Null operator% (Null, T const&) { return {}; }
    template <typename T> Null operator&&(Null, T const&) { return {}; }
    template <typename T> Null operator||(Null, T const&) { return {}; }
    template <typename T> Null operator^ (Null, T const&) { return {}; }

    template <typename T> Null operator+ (T const&, Null) { return {}; }
    template <typename T> Null operator- (T const&, Null) { return {}; }
    template <typename T> Null operator* (T const&, Null) { return {}; }
    template <typename T> Null operator/ (T const&, Null) { return {}; }
    template <typename T> Null operator% (T const&, Null) { return {}; }
    template <typename T> Null operator&&(T const&, Null) { return {}; }
    template <typename T> Null operator||(T const&, Null) { return {}; }
    template <typename T> Null operator^ (T const&, Null) { return {}; }

    template <typename T> Null operator< (Null, T const&) { return {}; }
    template <typename T> Null operator<=(Null, T const&) { return {}; }
    template <typename T> Null operator> (Null, T const&) { return {}; }
    template <typename T> Null operator>=(Null, T const&) { return {}; }
    template <typename T> Null operator==(Null, T const&) { return {}; }
    template <typename T> Null operator!=(Null, T const&) { return {}; }

    template <typename T> Null operator< (T const&, Null) { return {}; }
    template <typename T> Null operator<=(T const&, Null) { return {}; }
    template <typename T> Null operator> (T const&, Null) { return {}; }
    template <typename T> Null operator>=(T const&, Null) { return {}; }
    template <typename T> Null operator==(T const&, Null) { return {}; }
    template <typename T> Null operator!=(T const&, Null) { return {}; }

    static inline Null operator< (Null, Null) { return {}; }
    static inline Null operator<=(Null, Null) { return {}; }
    static inline Null operator> (Null, Null) { return {}; }
    static inline Null operator>=(Null, Null) { return {}; }
    static inline Null operator==(Null, Null) { return {}; }
    static inline Null operator!=(Null, Null) { return {}; }
    static inline Null operator- (Null) { return {}; }
    static inline Null operator! (Null) { return {}; }
}

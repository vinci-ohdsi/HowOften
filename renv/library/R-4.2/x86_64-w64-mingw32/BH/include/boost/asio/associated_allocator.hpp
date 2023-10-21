//
// associated_allocator.hpp
// ~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2022 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef BOOST_ASIO_ASSOCIATED_ALLOCATOR_HPP
#define BOOST_ASIO_ASSOCIATED_ALLOCATOR_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
# pragma once
#endif // defined(_MSC_VER) && (_MSC_VER >= 1200)

#include <boost/asio/detail/config.hpp>
#include <memory>
#include <boost/asio/associator.hpp>
#include <boost/asio/detail/functional.hpp>
#include <boost/asio/detail/type_traits.hpp>

#include <boost/asio/detail/push_options.hpp>

namespace boost {
namespace asio {

template <typename T, typename Allocator>
struct associated_allocator;

namespace detail {

template <typename T, typename = void>
struct has_allocator_type : false_type
{
};

template <typename T>
struct has_allocator_type<T,
  typename void_type<typename T::allocator_type>::type>
    : true_type
{
};

template <typename T, typename A, typename = void, typename = void>
struct associated_allocator_impl
{
  typedef void asio_associated_allocator_is_unspecialised;

  typedef A type;

  static type get(const T&) BOOST_ASIO_NOEXCEPT
  {
    return type();
  }

  static const type& get(const T&, const A& a) BOOST_ASIO_NOEXCEPT
  {
    return a;
  }
};

template <typename T, typename A>
struct associated_allocator_impl<T, A,
  typename void_type<typename T::allocator_type>::type>
{
  typedef typename T::allocator_type type;

  static BOOST_ASIO_AUTO_RETURN_TYPE_PREFIX(type) get(
      const T& t) BOOST_ASIO_NOEXCEPT
    BOOST_ASIO_AUTO_RETURN_TYPE_SUFFIX((t.get_allocator()))
  {
    return t.get_allocator();
  }

  static BOOST_ASIO_AUTO_RETURN_TYPE_PREFIX(type) get(
      const T& t, const A&) BOOST_ASIO_NOEXCEPT
    BOOST_ASIO_AUTO_RETURN_TYPE_SUFFIX((t.get_allocator()))
  {
    return t.get_allocator();
  }
};

template <typename T, typename A>
struct associated_allocator_impl<T, A,
  typename enable_if<
    !has_allocator_type<T>::value
  >::type,
  typename void_type<
    typename associator<associated_allocator, T, A>::type
  >::type> : associator<associated_allocator, T, A>
{
};

} // namespace detail

/// Traits type used to obtain the allocator associated with an object.
/**
 * A program may specialise this traits type if the @c T template parameter in
 * the specialisation is a user-defined type. The template parameter @c
 * Allocator shall be a type meeting the Allocator requirements.
 *
 * Specialisations shall meet the following requirements, where @c t is a const
 * reference to an object of type @c T, and @c a is an object of type @c
 * Allocator.
 *
 * @li Provide a nested typedef @c type that identifies a type meeting the
 * Allocator requirements.
 *
 * @li Provide a noexcept static member function named @c get, callable as @c
 * get(t) and with return type @c type or a (possibly const) reference to @c
 * type.
 *
 * @li Provide a noexcept static member function named @c get, callable as @c
 * get(t,a) and with return type @c type or a (possibly const) reference to @c
 * type.
 */
template <typename T, typename Allocator = std::allocator<void> >
struct associated_allocator
#if !defined(GENERATING_DOCUMENTATION)
  : detail::associated_allocator_impl<T, Allocator>
#endif // !defined(GENERATING_DOCUMENTATION)
{
#if defined(GENERATING_DOCUMENTATION)
  /// If @c T has a nested type @c allocator_type, <tt>T::allocator_type</tt>.
  /// Otherwise @c Allocator.
  typedef see_below type;

  /// If @c T has a nested type @c allocator_type, returns
  /// <tt>t.get_allocator()</tt>. Otherwise returns @c type().
  static decltype(auto) get(const T& t) BOOST_ASIO_NOEXCEPT;

  /// If @c T has a nested type @c allocator_type, returns
  /// <tt>t.get_allocator()</tt>. Otherwise returns @c a.
  static decltype(auto) get(const T& t, const Allocator& a) BOOST_ASIO_NOEXCEPT;
#endif // defined(GENERATING_DOCUMENTATION)
};

/// Helper function to obtain an object's associated allocator.
/**
 * @returns <tt>associated_allocator<T>::get(t)</tt>
 */
template <typename T>
BOOST_ASIO_NODISCARD inline typename associated_allocator<T>::type
get_associated_allocator(const T& t) BOOST_ASIO_NOEXCEPT
{
  return associated_allocator<T>::get(t);
}

/// Helper function to obtain an object's associated allocator.
/**
 * @returns <tt>associated_allocator<T, Allocator>::get(t, a)</tt>
 */
template <typename T, typename Allocator>
BOOST_ASIO_NODISCARD inline BOOST_ASIO_AUTO_RETURN_TYPE_PREFIX2(
    typename associated_allocator<T, Allocator>::type)
get_associated_allocator(const T& t, const Allocator& a) BOOST_ASIO_NOEXCEPT
  BOOST_ASIO_AUTO_RETURN_TYPE_SUFFIX((
    associated_allocator<T, Allocator>::get(t, a)))
{
  return associated_allocator<T, Allocator>::get(t, a);
}

#if defined(BOOST_ASIO_HAS_ALIAS_TEMPLATES)

template <typename T, typename Allocator = std::allocator<void> >
using associated_allocator_t
  = typename associated_allocator<T, Allocator>::type;

#endif // defined(BOOST_ASIO_HAS_ALIAS_TEMPLATES)

namespace detail {

template <typename T, typename A, typename = void>
struct associated_allocator_forwarding_base
{
};

template <typename T, typename A>
struct associated_allocator_forwarding_base<T, A,
    typename enable_if<
      is_same<
        typename associated_allocator<T,
          A>::asio_associated_allocator_is_unspecialised,
        void
      >::value
    >::type>
{
  typedef void asio_associated_allocator_is_unspecialised;
};

} // namespace detail

#if defined(BOOST_ASIO_HAS_STD_REFERENCE_WRAPPER) \
  || defined(GENERATING_DOCUMENTATION)

/// Specialisation of associated_allocator for @c std::reference_wrapper.
template <typename T, typename Allocator>
struct associated_allocator<reference_wrapper<T>, Allocator>
#if !defined(GENERATING_DOCUMENTATION)
  : detail::associated_allocator_forwarding_base<T, Allocator>
#endif // !defined(GENERATING_DOCUMENTATION)
{
  /// Forwards @c type to the associator specialisation for the unwrapped type
  /// @c T.
  typedef typename associated_allocator<T, Allocator>::type type;

  /// Forwards the request to get the allocator to the associator specialisation
  /// for the unwrapped type @c T.
  static type get(reference_wrapper<T> t) BOOST_ASIO_NOEXCEPT
  {
    return associated_allocator<T, Allocator>::get(t.get());
  }

  /// Forwards the request to get the allocator to the associator specialisation
  /// for the unwrapped type @c T.
  static BOOST_ASIO_AUTO_RETURN_TYPE_PREFIX(type) get(
      reference_wrapper<T> t, const Allocator& a) BOOST_ASIO_NOEXCEPT
    BOOST_ASIO_AUTO_RETURN_TYPE_SUFFIX((
      associated_allocator<T, Allocator>::get(t.get(), a)))
  {
    return associated_allocator<T, Allocator>::get(t.get(), a);
  }
};

#endif // defined(BOOST_ASIO_HAS_STD_REFERENCE_WRAPPER)
       //   || defined(GENERATING_DOCUMENTATION)

} // namespace asio
} // namespace boost

#include <boost/asio/detail/pop_options.hpp>

#endif // BOOST_ASIO_ASSOCIATED_ALLOCATOR_HPP
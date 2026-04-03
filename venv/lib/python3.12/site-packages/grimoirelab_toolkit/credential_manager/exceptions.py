# -*- coding: utf-8 -*-
#
# Copyright (C) Grimoirelab Contributors
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#
# Author:
#     Alberto Ferrer Sánchez (alberefe@gmail.com)
#

"""Custom exceptions for the credential manager module."""

__all__ = [
    "CredentialManagerError",
    "InvalidCredentialsError",
    "CredentialNotFoundError",
    "BitwardenCLIError",
]


class CredentialManagerError(Exception):
    """Base exception for all credential manager errors."""

    pass


class InvalidCredentialsError(CredentialManagerError):
    """Raised when invalid credentials are provided."""

    pass


class CredentialNotFoundError(CredentialManagerError):
    """Raised when a specific credential is not found in a secret."""

    pass


class BitwardenCLIError(CredentialManagerError):
    """Raised for Bitwarden CLI specific errors."""

    pass

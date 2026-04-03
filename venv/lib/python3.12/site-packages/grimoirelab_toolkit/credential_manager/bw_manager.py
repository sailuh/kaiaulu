#
#
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
#       Alberto Ferrer Sánchez (alberefe@gmail.com)
#
import json
import subprocess
import logging
import shutil

from .exceptions import (
    BitwardenCLIError,
    InvalidCredentialsError,
    CredentialNotFoundError,
)

logger = logging.getLogger(__name__)


class BitwardenManager:
    """Retrieve credentials from Bitwarden.

    This class defines functions to log in, retrieve secrets
    and log out of Bitwarden using the Bitwarden CLI. The
    workflow is:

    manager = BitwardenManager(client_id, client_secret, master_password)
    manager.login()
    manager.get_secret("github")
    manager.get_secret("elasticsearch")
    manager.logout()

    The manager logs in using the client_id, client_secret, and
    master_password given as arguments when creating the instance,
    so the object is reusable along the program.

    The path of Bitwarden CLI (bw) is retrieved using shutil.
    """

    def __init__(self, client_id: str, client_secret: str, master_password: str):
        """
        Creates BitwardenManager object using API key authentication

        :param str client_id: Bitwarden API client ID
        :param str client_secret: Bitwarden API client secret
        :param str master_password: Master password for unlocking the vault
        """
        # Session key of the bw session
        self.session_key = None

        # API credentials
        self.client_id = client_id
        self.client_secret = client_secret
        self.master_password = master_password

        # Get the absolute path to the bw executable
        self.bw_path = shutil.which("bw")
        if not self.bw_path:
            raise BitwardenCLIError("Bitwarden CLI (bw) not found in PATH")

        # Set up environment variables for consistent execution context
        self.env = {
            "LANG": "C",
            "BW_CLIENTID": client_id,
            "BW_CLIENTSECRET": client_secret,
        }

    def login(self) -> str | None:
        """Log into Bitwarden.

        Use the API authentication key to log in and unlock the vault. After it,
        it will obtain a session key that will be used by to access the vault.

        :returns: The session key for the current Bitwarden session.

        :raises InvalidCredentialsError: If invalid credentials are provided
        :raises BitwardenCLIError: If Bitwarden CLI operations fail
        """
        # Log in using API key
        login_result = subprocess.run(
            [self.bw_path, "login", "--apikey"],
            input=f"{self.client_id}\n{self.client_secret}\n",
            capture_output=True,
            text=True,
            env=self.env,
        )

        if login_result.returncode != 0:
            error_msg = (
                login_result.stderr.strip() if login_result.stderr else "Unknown error"
            )
            logger.error("Error logging in with API key: %s", error_msg)
            raise InvalidCredentialsError(
                "Invalid API credentials provided for Bitwarden"
            )

        # After login, we need to unlock the vault to get a session key
        self.session_key = self._unlock_vault()

        return self.session_key

    def _unlock_vault(self) -> str:
        """Unlock the vault after authentication.

        Executes the Bitwarden unlock command to obtain a session key
        for an already authenticated user but locked vault.

        :returns: Session key for the unlocked vault
        :raises BitwardenCLIError: If unlock operation fails or returns empty session key
        """
        # this uses the master password to unlock the vault
        unlock_result = subprocess.run(
            [self.bw_path, "unlock", "--raw"],
            input=f"{self.master_password}\n",
            capture_output=True,
            text=True,
            env=self.env,
        )

        if unlock_result.returncode != 0:
            error_msg = (
                unlock_result.stderr.strip()
                if unlock_result.stderr
                else "Unknown error"
            )
            logger.error("Error unlocking vault: %s", error_msg)
            raise BitwardenCLIError(f"Failed to unlock vault: {error_msg}")

        # the session key is used when retrieving the secrets with get_secret
        session_key = unlock_result.stdout.strip()
        if not session_key:
            raise BitwardenCLIError("Empty session key received from unlock command")

        return session_key

    def get_secret(self, item_name: str) -> dict:
        """Retrieve an item from the Bitwarden vault.

        Retrieves all the fields stored for an item with the name
        provided as an argument and returns them as a dictionary.

        The returned dictionary includes fields such as:
        - login: username, password, URIs, TOTP
        - fields: custom fields
        - notes: secure notes
        - name, id, and other metadata

        :param str item_name: The name of the item to retrieve

        :returns: Dictionary containing the item data
        :rtype: dict

        :raises CredentialNotFoundError: If the specific credential is not found
        :raises BitwardenCLIError: If Bitwarden CLI operations fail
        """
        # Pass session key via command line parameter
        result = subprocess.run(
            [self.bw_path, "get", "item", item_name, "--session", self.session_key],
            capture_output=True,
            text=True,
            env=self.env,
        )

        if result.returncode != 0:
            raise CredentialNotFoundError(f"Credential not found: '{item_name}'")

        # Parse the JSON response returned in stdout
        try:
            item = json.loads(result.stdout)
        except json.JSONDecodeError as e:
            logger.error("Failed to parse Bitwarden response: %s", str(e))
            raise BitwardenCLIError(f"Invalid JSON response from Bitwarden: {e}")

        return item

    def logout(self) -> None:
        """Log out from Bitwarden and invalidate the session.

        This method ends the current session and clears the session key.
        """
        logger.info("Logging out from Bitwarden")

        # Execute logout command
        result = subprocess.run(
            [self.bw_path, "logout"],
            capture_output=True,
            text=True,
            env=self.env,
        )

        if result.returncode != 0:
            error_msg = result.stderr.strip() if result.stderr else "Unknown error"
            logger.error("Error during logout: %s", error_msg)

        # Clear session key for security
        self.session_key = None

        logger.info("Successfully logged out from Bitwarden")

exports.auth0Config = {
  domain: process.env.AUTH0_DOMAIN,
  clientID: process.env.AUTH0_CLIENT_ID,
  audience: process.env.AUTH0_AUDIENCE,
  scope: process.env.AUTH0_SCOPE,
  redirectUri: process.env.AUTH0_REDIRECT_URI,
  responseType: "token id_token",
};

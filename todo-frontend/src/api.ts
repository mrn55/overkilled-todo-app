import axios from 'axios';

declare global {
  interface Window {
    __TODO_CONFIG__?: {
      apiBaseUrl?: string;
    };
  }
}

const trimTrailingSlash = (value: string) => value.replace(/\/+$/, '');

const resolveApiBaseUrl = () => {
  const configuredUrl = window.__TODO_CONFIG__?.apiBaseUrl || import.meta.env.VITE_API_BASE_URL;

  if (configuredUrl) {
    return trimTrailingSlash(configuredUrl);
  }

  if (window.location.hostname === 'localhost' && window.location.port === '3000') {
    return 'http://localhost:8081/todo';
  }

  return '/todo';
};

export const apiBaseUrl = resolveApiBaseUrl();

export const todoApi = axios.create({
  baseURL: apiBaseUrl,
});

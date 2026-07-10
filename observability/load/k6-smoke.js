import http from "k6/http";
import { check, sleep } from "k6";

export const options = {
  vus: 1,
  iterations: 5,
  thresholds: {
    http_req_failed: ["rate<0.01"],
    http_req_duration: ["p(95)<1000"],
  },
};

const baseUrl = __ENV.BASE_URL || "http://todo-dev.example.com";
const hostHeader = __ENV.HOST_HEADER;

function params() {
  return hostHeader ? { headers: { Host: hostHeader } } : {};
}

export default function () {
  const res = http.get(`${baseUrl}/todo`, params());
  check(res, {
    "GET /todo is 200": (r) => r.status === 200,
    "GET /todo returns JSON": (r) => r.headers["Content-Type"] && r.headers["Content-Type"].includes("application/json"),
  });
  sleep(1);
}

import useSWR from "swr";
import React from "react";

import { Card, BuildInfo } from "@/card.tsx";
import * as Api from "@/api.ts";

export default () => {
  const base = Api.endpoint + "/build";
  const { data, error } = useSWR<Array<Api.Job>>(
    base,
    Api.fetchJson,
    Api.refresh
  );

  if (error) return <div>Failed to load</div>;
  if (!data) return <div>Loading</div>;

  return (
    <main>
      <ul>
        {data.map((job) => {
          return (
            <li className="mb-5" key={job.number}>
              <Card>
                <BuildInfo job={job} />
              </Card>
            </li>
          );
        })}
      </ul>

      {data.length == 0 && <Help />}
    </main>
  );
};

const Help = () => {
  return (
    <div className="max-w-md mx-auto">
      <Card>
        <div className="py-5 px-3">
          <h2 className="font-bold">There are no builds here! 🤯</h2>
          <br />
          <p>
            You can trigger one by pushing a commit to a repo with a{" "}
            <code>.quad.yml</code> file. Setup a webhook to{" "}
            <code>{Api.endpoint}/webhook/github</code> or use a proxy like
            ngrok.
          </p>
          <br />
          <p>
            Does that sound like too much work? 🤓 <br />
            You can use the sample repo{" "}
            <code>alpacaaa/quad-sample-pipeline</code> and simulate the webhook
            payload by sending this request to your api server!
          </p>
          <br />
          <pre className="max-w-md mt-3 p-4 rounded bg-gray-200 font-mono text-sm whitespace-pre-wrap">
            <code>
              curl -X POST -H "Content-Type: application/json" -d
              @test/github-payload.sample.json "{Api.endpoint}/webhook/github"
            </code>
          </pre>
        </div>
      </Card>
    </div>
  );
};
